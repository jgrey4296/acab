#!/usr/bin/env python3
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from functools import reduce
from re import Pattern
from typing import (Any, Callable, ClassVar, Collection, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, Sequence, Tuple,
                    Type, TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab.core.data.default_structure as DS
import acab.core.util.part_implementations.value as VSI  # type:ignore
import acab.interfaces.protocols.value as VP
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.factory import ValueFactory
from acab.core.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve

logging        = logmod.getLogger(__name__)

config         = AcabConfig()
BIND_SYMBOL    = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP      = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T              = TypeVar('T', bound=AT.ValueCore)

Value_A       : TypeAlias = AT.Value
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
ValueData     : TypeAlias = str

class _SentenceBasicsImpl(VI.Sentence_i, VSI._ValueBasicsImpl, VP.ValueBasics_p):
    """
    Utility class for basic instruction/sentence methods
    """
    @cache
    def __str__(self):
        return "{}".format(FALLBACK_MODAL.join([str(x) for x in self.words]))

    @cache
    def __repr__(self):
        name_str = self.key()
        val_str  = str(self)

        if self.is_at_var:
            name_str = BIND_SYMBOL + name_str
        elif self.is_var:
            name_str = BIND_SYMBOL + name_str

        type_str = str(self.type)

        return "<{}::{} [{}]>".format(name_str, type_str, val_str)

    def __eq__(self, other):
        if isinstance(other, str) and other[:2] == "_:":
            # Utility for str comparison. underscore colon signifies sen str
            return str(self) == other[2:]
        elif isinstance(other, str):
            # If not a sen str, just compare to the name
            return self.key() == other
        elif not isinstance(other, VI.Sentence_i):
            # anything not a sentence isn't equal
            return False
        elif len(self) != len(other):
            # Lengths not equal mean difference
            return False
        elif len(self.params) != len(other.params):
            return False
        elif not all([x == y for x,y in zip(self.params, other.params)]):
            return False
        else:
            # finally, words match exactly
            return all([x == y for x,y in zip(self, other)])


    def copy(self, **kwargs) -> Sen_A:
        if 'value' not in kwargs:
            assert(isinstance(self.value, Iterable))
            kwargs['value'] = [x.copy() if hasattr(x, 'copy') else x for x in cast(Iterable, self.value)]

        if 'params' not in kwargs:
            kwargs['params'] = self.params.copy()
        if 'tags' in kwargs:
            kwargs['tags'] = set(kwargs['tags']) | self.tags
        if 'data' not in kwargs:
            kwargs['data'] = self.data.copy()
        else:
            temp = self.data.copy()
            temp.update(kwargs['data'])
            kwargs['data'] = temp

        return replace(self, uuid=uuid1(), **kwargs) #type:ignore


class _SentenceVariableTestsImpl(VI.Sentence_i, VP.VariableTests_p):
    @property
    def is_var(self) -> bool:
        if not bool(self):
            return False
        if len(self) > 1:
            return False

        return cast(bool, self[0].is_var)

    @property
    def is_at_var(self) -> bool:
        if not bool(self):
            return False
        if len(self) > 1:
            return False

        return cast(bool, self[0].is_at_var)

    @property
    def has_var(self) -> bool:
        if not bool(self):
            return False
        if self.is_var:
            return True
        return any([x.is_var for x in self.words])

    @property
    def vars(self) -> list[Value_A]:
        return [x for x in self.words if x.is_var]


class _SentenceCollectionImpl(VI.Sentence_i, Collection):

    def __contains__(self, value) -> bool:
        assert(isinstance(value, (str, VI.Value_i)))
        words = cast(list[VI.Value_i], self.words)
        return value in words or value in [x.name for x in words]

    def __len__(self):
        return len(self.words)

    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return ValueFactory.sen(self.words.__getitem__(i), data=self.data)

        if isinstance(i, str):
            matches = [x for x in self.words if x.name == i]
            return matches[0]

        if isinstance(i, int):
            return self.words.__getitem__(i)

        raise ValueError("Unrecognised argument to Sentence.__getitem__", i)

class _SentenceReductionImpl(VI.Sentence_i, VP.AcabReducible_p):
    def attach_statement(self, value:Instruction_A) -> Sen_A:
        """
        for this S: first..last
        and value V,
        produce S' where last = V
        such that statement S still matches

        Copy the sentence,
        Replace the leaf with the provided statement,
        Name the statement to the name of the former leaf
        return modified copy
        """
        assert(isinstance(value, VI.Value_i))
        last = cast(VI.Value_i, self.words[-1])
        combined_data = last.data.copy()
        combined_data.update(value.data)
        combined_tags = value.tags | last.tags

        value_copy = cast(VI.Value_i, value).copy(name=last.name, data=combined_data,
                                                  tags=combined_tags)
        new_words = self.words[:-1] + [value_copy]
        sen_copy = self.copy(value=new_words) #type:ignore

        return sen_copy #type:ignore

    def detach_statement(self) -> Tuple[VI.Instruction_i, list[Instruction_A]]:
        """
        The inverse of attach_statement.
        Copy the sentence,
        Reduce all words down to basic values
        Return modified copy, and the statement
        """
        statements = []
        out_words  = []

        # collect leaf statements
        for word in self.words:
            if isinstance(word, VI.Instruction_i):
                out_words.append(word.to_word())
                statements.append(word)
            else:
                out_words.append(word)

        sen_copy = self.copy(value=out_words) #type:ignore
        return (sen_copy, statements)


    def to_sentences(self) -> list[Sen_A]:
        simple_sen, statements = self.detach_statement()
        # TODO turn statements to sentences
        return [simple_sen] #type:ignore

    @staticmethod
    def from_sentences(self, sens:list[Sen_A]) -> list[Instruction_A]:
        return cast(list[Instruction_A], sens)



    def to_word(self) -> Value_A:
        """ Convert a Statement to just an AcabValue, of it's name """

        new_data = {}
        new_data.update(self.data)
        new_data.update({DS.TYPE_INSTANCE: ValueFactory.sen([DS.TYPE_BASE])}) #type:ignore
        simple_value = ValueFactory.value(self.name, data=new_data) #type:ignore
        return simple_value #type:ignore

class SentenceProtocolsImpl(_SentenceBasicsImpl, VSI._ValueMetaDataImpl, _SentenceVariableTestsImpl, _SentenceCollectionImpl, _SentenceReductionImpl):

    def add(self, *other) -> Sen_A:
        """ Return a copy of the sentence, with words added to the end.
        This can flatten entire sentences onto the end
        return modified copy
        """
        words = self.words[:]
        for sen in other:
            assert(isinstance(sen, (list, VI.Sentence_i)))
            words += [x for x in sen]

        new_sen = replace(self, value=words)
        return new_sen


    def clear(self) -> Sen_A:
        """
        return modified copy
        """
        return self.copy(value=[]) #type:ignore

    def prefix(self, prefix:'Value_A|Sen_A|list') -> Sen_A:
        """
        For prefix P, this S produces P..S
        """
        if isinstance(prefix, list):
            prefix = ValueFactory.sen(prefix)
        elif not isinstance(self, VI.Sentence_i):
            prefix = ValueFactory.sen([prefix])

        return cast(VI.Sentence_i, prefix).add(self)


    def remove_prefix(self, prefix:'Value_A|Sen_A|list') -> Sen_A:
        if not isinstance(prefix, (VI.Sentence_i, list)):
            prefix = [prefix]

        if all([x==y for x,y in zip(prefix, self)]):
            return self.copy(value=cast(list, self.value)[len(prefix):]) #type:ignore

        return self

    def flatten(self, *, rec=False) -> Sen_A:
        """
        TODO: add annotations for:
        flattening sentences
        flattening a variable in a sentence
        recursive flattening
        """
        if DS.FLATTEN in self.data and not self.data[DS.FLATTEN]:
            return self

        words = []
        queue = self.words
        while bool(queue):
            word = queue.pop(0)
            is_sen = isinstance(word, VI.Sentence_i)
            should_flatten = DS.FLATTEN not in word.data or bool(word.data[DS.FLATTEN])
            if is_sen and should_flatten and rec:
                queue = word.words + queue
            elif is_sen and should_flatten:
                words += word.words
            else:
                assert(not (is_sen and should_flatten))
                words.append(word)
        return replace(self, value=words)

