#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from functools import reduce
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Collection, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, Sequence, Tuple,
                    Type, TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab.core.defaults.value_keys as DS
import acab.core.value.part_implementations.value as VSI  # type:ignore
import acab.interfaces.protocols.value as VP
import acab.interfaces.value as VI
from acab import types as AT
from acab import AcabConfig
from acab.core.util.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.context import ContextInstance_i
from acab.interfaces.sieve import AcabSieve
from acab.interfaces.value import ValueFactory

logging        = logmod.getLogger(__name__)

if TYPE_CHECKING:
    Value_A       : TypeAlias = AT.Value
    Sen_A         : TypeAlias = AT.Sentence
    Instruction_A : TypeAlias = AT.Instruction
    ValueData     : TypeAlias = str

T              = TypeVar('T', bound=AT.ValueCore)

config         = AcabConfig()
BIND_SYMBOL    = config.attr.Symbols.BIND
FALLBACK_MODAL = config.attr.Symbols.FALLBACK_MODAL

UUID_CHOP  = config.prepare("Print.Data", "UUID_CHOP", _type=bool)
ANON_VALUE = config.attr.Symbols.ANON_VALUE

class _SentenceBasicsImpl(VI.Sentence_i, VSI._ValueBasicsImpl, VP.ValueBasics_p):
    """
    Utility class for basic instruction/sentence methods
    """
    @cache
    def __str__(self):
        if self.name != ANON_VALUE:
            return f"'{self.name}'"

        words = [f"{x}" for x in self.words]
        return "[{}]".format(FALLBACK_MODAL.join(words))

    @cache
    def __repr__(self):
        if len(self) == 1 and self.type == "_:SENTENCE":
            return f"<[{self[0]}]>"

        name_str = self.name
        words    = [f"{x}" for x in self.words]
        val_str  = "[{}]".format(FALLBACK_MODAL.join(words))

        type_str = "::" + str(self.type) if self.type != "_:SENTENCE" else ""


        return "<{}{} {}>".format(name_str, type_str, val_str)

    def __eq__(self, other):
        match other:
            case str() if other[:2] == "_:":
                # Utility for str comparison. underscore colon signifies sen str
                return str(self) == f"[{other[2:]}]"
            case str():
                # If not a sen str, just compare to the key
                return self.key() == other
            case VI.Sentence_i() if len(self) != len(other):
                return False
            case VI.Sentence_i() if len(self.params) != len(other.params):
                return False
            case VI.Sentence_i() if all([x == y for x,y in zip(self, other)]):
                return True
            case _:
                return False


    def key(self, suffix=None) -> str:
        # if self.is_var:
        if len(self) == 1:
            return self[0].key()

        words = [f"{x.key()}" for x in self.words]
        key   = "[{}]".format(FALLBACK_MODAL.join(words))
        if suffix is None:
            return key

        suffix_key = suffix if isinstance(suffix, str) else suffix.key()

        # Splice the suffix in
        key_mod = key[:-1] + (FALLBACK_MODAL if bool(self) else "") + suffix_key + "]"
        return key_mod

    def copy(self, **kwargs) -> Sen_A:
        # if 'value' not in kwargs:
        #     assert(isinstance(self.value, Iterable))
        #     kwargs['value'] = [x.copy() if hasattr(x, 'copy') else x for x in cast(Iterable, self.value)]

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
    @cache
    def is_var(self) -> bool:
        if not bool(self):
            return False
        if len(self) > 1:
            return False
        if isinstance(self[0], VI.Sentence_i):
            return False

        return cast(bool, self[0].is_var)

    @property
    @cache
    def is_at_var(self) -> bool:
        if not bool(self):
            return False
        if len(self) > 1:
            return False

        return cast(bool, self[0].is_at_var)

    @property
    @cache
    def has_var(self) -> bool:
        if not bool(self):
            return False
        if self.is_var:
            return True
        return any([x.has_var for x in self.words])

    @property
    def vars(self) -> list[Value_A]:
        return [x for x in self.words if x.is_var]


class _SentenceCollectionImpl(VI.Sentence_i, Collection):

    def __contains__(self, value) -> bool:
        match value:
            case str():
                return value in [x.name for x in self.words]
            case VI.Sentence_i():
                return value.key() in self.words
            case VI.Value_i():
                return value.key() in self.words
            case _:
                raise TypeError("Unknown type passed to Sentence.contains")

    @cache
    def __len__(self):
        return len(self.words)

    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i,):
        match i:
            case slice():
                return self.copy(value=self.words.__getitem__(i))
            case str():
                matches = [x for x in self.words if x.name == i]
                return matches[0]
            case int():
                return self.words.__getitem__(i)
            case tuple() if len(i) == 1 and isinstance(i[0], slice):
                return self.copy(value=self.words.__getitem__(i[0]))
            case tuple() if all([isinstance(x, slice) for x in i]):
                words = self.words.__getitem__(i[0])
                words.append(words.pop()[i[1:]])
                return self.copy(value=words)
            case _:
                raise ValueError("Unrecognised argument to Sentence.__getitem__", i)

class _SentenceReductionImpl(VI.Sentence_i):
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
        match prefix:
            case list():
                return self.copy(value=prefix+self.words)
            case VI.Sentence_i():
                return prefix << self
            case VI.Value_i():
                return self.copy(value=[prefix]+self.words)


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

