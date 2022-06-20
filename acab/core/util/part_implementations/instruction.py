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

import acab.core.defaults.value_keys as DS
import acab.core.util.part_implementations.value as VSI  # type:ignore
import acab.interfaces.protocols.value as VP
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.interfaces.value import ValueFactory
from acab.core.util.decorators.util import cache
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

class _InstructionBasicsImpl(VI.Instruction_i, VSI._ValueBasicsImpl, VP.ValueBasics_p):
    """
    Utility class for basic instruction/sentence methods
    """
    @cache
    def __str__(self):
        # return "{}".format(FALLBACK_MODAL.join([str(x) for x in self.value]))
        return f"'{self.name}'"


    @cache
    def __repr__(self):
        type_str = f"::{self.type}" if self.type != f"_:{self.name}" else ""
        return f"<{self.key()}{type_str} ({len(self.value)})>"

    def __eq__(self, other):
        if isinstance(other, str) and other[:2] == "_:":
            # Utility for str comparison. underscore colon signifies sen str
            return str(self) == other[2:]
        elif isinstance(other, str):
            # If not a sen str, just compare to the name
            return self.key() == other
        elif not isinstance(other, VI.Instruction_i):
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
            # finally, clauses match exactly
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


class _InstructionVariableTestsImpl(VI.Instruction_i, VP.VariableTests_p):
    @property
    def is_var(self) -> bool:
        return False
    @property
    def is_at_var(self) -> bool:
        return False
    @property
    def has_var(self) -> bool:
        if not bool(self):
            return False
        if self.is_var:
            return True
        return any([x.is_var for x in self.params]) or any([x.has_var for x in self.value])

    @property
    def vars(self) -> list[Value_A]:
        return {x for x in self.value if x.is_var} + {x for x in self.params if x.is_var}


class _InstructionCollectionImpl(VI.Instruction_i, Collection):

    def __contains__(self, value) -> bool:
        assert(isinstance(value, (str, VI.Value_i)))
        val = cast(list[VI.Value_i], self.value)
        return value in val

    def __len__(self):
        return len(self.value)

    def __iter__(self):
        return iter(self.value)

    def __getitem__(self, i):
        if isinstance(i, (int, slice)):
            return self.value[i]

        raise ValueError("Unrecognised argument to Instruction.__getitem__", i)

class _InstructionReductionImpl(VI.Instruction_i):
    def attach_statement(self, value:Instruction_A) -> Sen_A:
        raise TypeError("Instructions can't attach statements to themselves")
    def detach_statement(self) -> Tuple[VI.Instruction_i, list[Instruction_A]]:
        raise TypeError("Instructions can't detach statements from themselves")

    def to_word(self) -> Value_A:
        """ Convert a Statement to just an AcabValue, of it's name """

        new_data = {}
        new_data.update(self.data)
        new_data.update({DS.TYPE_INSTANCE: ValueFactory.sen([DS.TYPE_BASE])}) #type:ignore
        simple_value = ValueFactory.value(self.name, data=new_data) #type:ignore
        return simple_value #type:ignore

class InstructionProtocolsImpl(_InstructionBasicsImpl, VSI._ValueMetaDataImpl, _InstructionVariableTestsImpl, _InstructionCollectionImpl, _InstructionReductionImpl):
    pass
