#!/usr/bin/env python3
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from functools import reduce
from re import Pattern
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, Type,
                    TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.util.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve
import acab.interfaces.protocols.value as VP
from acab.interfaces.value import ValueFactory
from acab.core.util.debugging import logdel

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

@logdel
class _ValueBasicsImpl(VI.Value_i, VP.ValueBasics_p):
    """
    A Utility class for default implementations of Value_i methods
    """
    def __str__(self):
        return str(self.name)

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
        if isinstance(other, VI.Value_i):
            return self.name < other.name
        elif isinstance(other, str):
            return self.name < other

        return TypeError("AcabValues can only be __lt__'d with AcabValues and Strings")

    def copy(self, **kwargs) -> VI.Value_i:
        if 'tags' in kwargs:
            kwargs['tags'] = set(kwargs['tags']) | self.tags

        if 'data' in kwargs:
            temp = self.data.copy()
            temp.update(kwargs['data'])
            kwargs['data'] = temp

        if 'name' in kwargs and kwargs['name'] is None:
            kwargs['name'] = str(self.value)

        return replace(self, uuid=uuid1(), **kwargs) #type:ignore


class _ValueMetaDataImpl(VI.Value_i, VP.ValueMetaData_p):
    """
    Utility Class providing methods for handling value meta data
    """
    def key(self) -> str:
        return str(self.name)

    @property #type:ignore
    def type(self) -> Sen_A:
        """ Lazily coerces type description to Sentence """
        type_desc = self.data[DS.TYPE_INSTANCE]
        assert(type_desc is not None)
        type_matches_t = isinstance(type_desc, VI.Sentence_i)
        if type_matches_t:
            return type_desc # type:ignore

        assert(isinstance(type_desc, (DS.DATA_STRUCT_E, str)))
        if DS.SEMANTIC_HINT in self.data and isinstance(self.data[DS.SEMANTIC_HINT], VI.Sentence_i):
            self.data[DS.TYPE_INSTANCE] = self.data[DS.SEMANTIC_HINT]
        else:
            self.data[DS.TYPE_INSTANCE] = ValueFactory.sen([type_desc])

        return self.data[DS.TYPE_INSTANCE] # type:ignore

    def apply_params(self, *params, data=None) -> Value_A:
        """
        return modified copy
        """
        if not bool(params):
            return self #type:ignore

        # TODO check params against self.params
        safe_params = [x if isinstance(x, VI.Value_i) else ValueFactory.value(x) for x in params]
        return self.copy(params=params) #type:ignore

    def apply_tags(self, *tags, data=None) -> Value_A:
        """
        return modified copy
        """
        assert(all([isinstance(x, VI.Value_i) for x in tags]))
        if not bool(tags):
            return cast(VI.Value_i, self)

        tag_extension  = {x for x in self.tags}
        for tag in tags:
            if isinstance(tags, VI.Sentence_i):
                tag_extension.update(tag.words)
            else:
                tag_extension.update(tag)
        return self.copy(tags=tag_extension) #type:ignore


    def has_tag(self, *tags:Value_A) -> bool:
        return all([t in self.tags for t in tags])

class _VariableTestsImpl(VI.Value_i, VP.VariableTests_p):
    """
    Utility Class providing methods
    for checking if a value is a value
    """
    @property #type:ignore
    def is_var(self) -> bool:
        return self.data[DS.BIND] is not False

    @property #type:ignore
    def is_at_var(self) -> bool:
        return self.data[DS.BIND] == DS.AT_BIND #type:ignore

    @property #type:ignore
    def has_var(self) -> bool:
        if self.is_var:
            return True
        if any([x.has_var for x in self.params]):
            return True
        if any([x.has_var for x in self.tags]):
            return True
        if isinstance(self.value, VI.Value_i) and self.value.has_var:
            return True

        return False


class _WordLiftingImpl(VI.Value_i, VP.AcabReducible_p):
    """
    Utility class to provide methods for lifting and dropping values <->instructions
    """
    def attach_statement(self, value:Instruction_A) -> VI.Sentence_i:
        """
        Attach an unnamed statement to this value.
        Name the statement to the name of the former leaf
        return modified copy
        """
        assert(isinstance(value, VI.Value_i))
        combined_data = self.data.copy()
        combined_data.update(value.data)
        value_copy = value.copy(name=self.name, data=combined_data)
        return cast(VI.Sentence_i, value_copy)


    def to_word(self) -> Value_A:
        return cast(VI.Value_i, self)

    def detach_statement(self):
        return (self, self)
    def from_sentences(self):
        pass
    def to_sentences(self):
        pass



class ValueProtocolsImpl(_ValueBasicsImpl,
                         _ValueMetaDataImpl,
                         _VariableTestsImpl,
                         _WordLiftingImpl):
    """
    Utility class collecting together the subprotocol implementations
    """
    pass
