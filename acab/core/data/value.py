"""
The Core Value Classes: AcabValue, Instruction, Sentence
"""
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from re import Pattern
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence,
                    Tuple, TypeVar, cast, TypeAlias, Type)
from uuid import UUID, uuid1
from functools import reduce
from weakref import ref

import acab.core.data.default_structure as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve
from acab.core.data.util import name_sieve_fns

logging          = root_logger.getLogger(__name__)

config           = AcabConfig.Get()
BIND_SYMBOL      = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL   = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP        = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T           = TypeVar('T', bound=VI.ValueCore)

Value       : TypeAlias = AT.Value
Sen         : TypeAlias = AT.Sentence
Instruction : TypeAlias = AT.Instruction
ValueData   : TypeAlias = AT.ValueData

VI.ValueCore |= VI.Value_i

name_sieve = AcabSieve(name_sieve_fns)

@dataclass(frozen=True)
class AcabValue(VI.Value_i, Generic[T]):

    @classmethod
    def build(cls, value: T, *,
                  name: str=None,
                  data: None|dict[ValueData, Any]=None,
                  _type: None|Sen=None,
                  **kwargs) -> Value:
        """ Idempotent construction.
        Wrap the provided value in an AcabValue,
        but only if it isn't an AcabValue already """
        _data = {}
        if data is not None:
            _data.update(data)
        if _type is not None:
            _data.update({DS.TYPE_INSTANCE: _type})

        if isinstance(value, VI.Value_i):
            new_data = {}
            new_data.update(value.data)
            new_data.update(_data)
            name = name or value.name
            return value.copy(data=new_data, name=name)


        return cls(value=value, name=name, data=_data, **kwargs)

    def __post_init__(self):
        if not isinstance(self.value, VI.ValueCore):
            raise TypeError("AcabValue must wrap a valid core type", self.value)

        # NOTE: use of setattr to override frozen temporarily to update name and value
        # name update #########################################################
        name_update = name_sieve.fifo_first(self)
        object.__setattr__(self, "name", name_update)
        # self.name = name_update
        assert(isinstance(self.name, str)), self.name

        if self.value is None:
            object.__setattr__(self, "value", self.name)
            # self.value = self.name

        if DS.TYPE_INSTANCE not in self.data:
            self.data[DS.TYPE_INSTANCE] = DS.TYPE_BOTTOM_NAME

        if DS.BIND not in self.data:
            self.data[DS.BIND] = False

        original_params = self.params[:]
        self.params.clear()
        self.params.extend([AcabValue.build(x, data={DS.BIND: True}) for x in original_params])


    @cache
    def __str__(self):
        """ the simplest representation of the value,
        for internal use.
        For reparseable output, use a PrintSemantics
        """
        return self.name

    @cache
    def __repr__(self):
        name_str = self.name

        if self.is_at_var:
            name_str = BIND_SYMBOL + name_str
        elif self.is_var:
            name_str = BIND_SYMBOL + name_str

        type_str = str(self.type)
        if self.type.is_var:
            type_str = BIND_SYMBOL + type_str

        return "<{}::{}>".format(name_str, type_str)

    @cache
    def __hash__(self):
        return hash(repr(self))

    def __eq__(self, other):
        """ Base eq: compare hashes  """
        if id(self) == id(other):
            return True
        elif isinstance(other, str):
            return str(self) == other
        elif isinstance(other, UUID):
            return self.uuid == other
        elif not isinstance(other, VI.Value_i):
            return False
        elif self.uuid == other.uuid:
            return True
        else:
            return str(self) == str(other)



    def __lt__(self, other):
        if isinstance(other, AcabValue):
            return self.name < other.name
        elif isinstance(other, str):
            return self.name < str

        return TypeError("AcabValues can only be __lt__'d with AcabValues and Strings")


    @cache
    def key(self):
        """ Default Key Value->str for node mapping """
        return str(self.name)

    @property
    @cache
    def type(self) -> Sen:
        """ Lazily coerces type description to Sentence """
        type_matches_t = isinstance(self.data[DS.TYPE_INSTANCE], Sentence)
        if type_matches_t:
            return self.data[DS.TYPE_INSTANCE]

        if DS.SEMANTIC_HINT in self.data and isinstance(self.data[DS.SEMANTIC_HINT], Sentence):
            self.data[DS.TYPE_INSTANCE] = self.data[DS.SEMANTIC_HINT]
        else:
            self.data[DS.TYPE_INSTANCE] = Sentence.build([self.data[DS.TYPE_INSTANCE]])

        return self.data[DS.TYPE_INSTANCE]


    @property
    @cache
    def is_var(self) -> bool:
        return self.data[DS.BIND] is not False

    @property
    @cache
    def is_at_var(self) -> bool:
        return self.data[DS.BIND] == DS.AT_BIND

    def copy(self, **kwargs) -> Value:
        """ copy the object, but give it a new uuid """
        if 'params' not in kwargs:
            kwargs['params'] = self.params[:]
        if 'tags' not in kwargs:
            kwargs['tags'] = self.tags.copy()

        if 'data' not in kwargs:
            kwargs['data'] = self.data.copy()
        else:
            temp = self.data.copy()
            temp.update(kwargs['data'])
            kwargs['data'] = temp

        return replace(self, uuid=uuid1(), **kwargs)


    def bind(self, bindings) -> Value:
        raise Exception("Deprecated: use acab.modules.values.binding")

    def apply_params(self, params, *, data=None) -> Value:
        """
        return modified copy
        """
        if not bool(params):
            return self

        # TODO check params against self.params
        safe_params = [x if isinstance(x, VI.Value_i) else AcabValue(x) for x in params]
        return self.copy(params=safe_params)

    def apply_tags(self, tags:list[Value]) -> Value:
        """
        return modified copy
        """
        assert(all([isinstance(x, VI.Value_i) for x in tags]))
        if not bool(tags):
            return self

        tag_extension  = {x for x in self.tags}
        tag_extension.update(tags)
        return self.copy(tags=tag_extension)

    def has_tag(self, *tags:list[Value]) -> bool:
        return all([t in self.tags for t in tags])


    def attach_statement(self, value) -> Instruction:
        """
        Attach an unnamed statement to this value.
        Name the statement to the name of the former leaf
        return modified copy
        """
        assert(isinstance(value, VI.Value_i))
        combined_data = self.data.copy()
        combined_data.update(value.data)
        value_copy = value.copy(name=self.name, data=combined_data)
        return value_copy



    @property
    def has_var(self) -> bool:
        if self.is_var:
            return True
        if any([x.has_var for x in self.params]):
            return True
        if any([x.has_var for x in self.tags]):
            return True
        return False

    def to_word(self) -> Value:
        return self

class Instruction(AcabValue, VI.Instruction_i):
    """ Instruction functions the same as AcabValue,
    but provides specific functionality for converting to/from sentences
    """

    def __repr__(self):
        return "<{}::{}>".format(self.name, str(self.type))

    def to_word(self) -> Value:
        """ Convert a Statement to just an AcabValue, of it's name """
        new_data = {}
        new_data.update(self.data)
        new_data.update({DS.TYPE_INSTANCE: Sentence.build([DS.TYPE_BOTTOM_NAME])})
        simple_value = AcabValue.build(self.name, data=new_data)
        return simple_value

    def to_sentences(self) -> list[VI.Sentence_i]:
        return []

    @staticmethod
    def from_sentences(self, sens:list[VI.Sentence]) -> list[VI.Instruction]:
        return sens

class Sentence(Instruction, VI.Sentence_i):
    """
    A Sentence is an instruction which is idempotent on from_sentences/to_sentences
    Sentence.from_sentences([sens]) == [sens]
    """

    @staticmethod
    def build(words, **kwargs):
        safe_words = [AcabValue.build(x) for x in words]
        sen = Sentence(value=safe_words, **kwargs)
        return sen


    def __post_init__(self):
        AcabValue.__post_init__(self)
        self.data[DS.TYPE_INSTANCE] = DS.SENTENCE_PRIM

    def __hash__(self):
        return AcabValue.__hash__(self)

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
        else:
            # finally, words match exactly
            return all([x == y for x,y in zip(self, other)])


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

    @cache
    def __str__(self):
        return "{}".format(FALLBACK_MODAL.join([str(x) for x in self.words]))

    @cache
    def __len__(self):
        return len(self.words)
    def __iter__(self):
        return iter(self.words)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Sentence.build(self.words.__getitem__(i), data=self.data)
        if isinstance(i, str):
            match = [x for x in self.words if x.name == i]
            return match[0]

        if isinstance(i, int):
            return self.words.__getitem__(i)

        raise ValueError("Unrecognised argument to Sentence.__getitem__", i)

    def __contains__(self, value:str|Value):
        return value in self.words or value in [x.name for x in self.words]

    @cache
    def key(self):
        return str(self.name)

    def copy(self, **kwargs) -> Sen:
        if 'value' not in kwargs:
            kwargs['value'] = [x.copy() for x in self.value]

        return super(Sentence, self).copy(**kwargs)

    def clear(self) -> Sen:
        """
        return modified copy
        """
        return self.copy(value=[])

    def bind(self, bindings:dict[Any, Any]) -> Sen:
        raise Exception("Deprecated: use acab.modules.values.binding")

    def add(self, *other) -> Sen:
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

    def prefix(self, prefix:Value|Sen|list) -> Sen:
        """
        For prefix P, this S produces P..S
        """
        if isinstance(prefix, list):
            prefix = Sentence.build(prefix)
        elif not isinstance(self, Sentence):
            prefix = Sentence.build([prefix])

        return prefix.add(self)


    def remove_prefix(self, prefix:Value|Sen|list) -> Sen:
        if not isinstance(prefix, (Sentence, list)):
            prefix = [prefix]

        if all([x==y for x,y in zip(prefix, self)]):
            return self.copy(value=self.value[len(prefix):])

        return self


    def attach_statement(self, value) -> Sen:
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
        last = self.words[-1]
        combined_data = last.data.copy()
        combined_data.update(value.data)
        value_copy = value.copy(name=last.name, data=combined_data)

        new_words = self.words[:-1] + [value_copy]
        sen_copy = self.copy(value=new_words)

        return sen_copy

    def detach_statement(self) -> Tuple[Sen, list[Instruction]]:
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

        sen_copy = self.copy(value=out_words)
        return (sen_copy, statements)


    @property
    @cache
    def is_var(self) -> bool:
        if len(self) > 1:
            return False

        return self[0].is_var

    @property
    @cache
    def is_at_var(self) -> bool:
        if len(self) > 1:
            return False

        return self[0].is_var

    @property
    def has_var(self) -> bool:
        if self.is_var:
            return True
        return any([x.is_var for x in self.words])


    def match(self, sen:Sen) -> list[Tuple[Value, Value]]:
        """ Match a target sentence's variables to self's target
        as tuples of (bind_name, value)

        eg: _:$x.b.$y match _:a.b.c -> [(x, a), (y, c)]
        """
        results = []
        if self.is_var:
            results.append((self[0], sen))
            return results

        for x,y in zip(self.words, sen.words):
            if x.is_var:
                results.append((x,y))

        return results


    def to_sentences(self) -> list[VI.Sentence_i]:
        simple_sen, statements = self.detach_statement()
        # TODO turn statements to sentences
        return [simple_sen]


    @property
    @cache
    def vars(self) -> list[Value]:
        return [x for x in self.words if x.is_var]
