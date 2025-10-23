#!/usr/bin/env python3
##-- imports
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from functools import reduce
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Collection,
                    Generic, Iterable, Iterator, Mapping, Match,
                    MutableMapping, Sequence, Tuple, Type, TypeAlias, TypeVar,
                    cast)
from uuid import UUID, uuid1
from weakref import ref

import acab
import acab.core.defaults.value_keys as DS
import acab.core.value.part_implementations.value as VSI  # type:ignore
import acab.interfaces.protocols.value as VP
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.util.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve
from acab.interfaces.value import ValueFactory

logging        = logmod.getLogger(__name__)

if TYPE_CHECKING:
    ValueData     : TypeAlias = str

##-- end imports

Value_A       : TypeAlias = VI.Value_i
Sen_A         : TypeAlias = VI.Sentence_i
Instruction_A : TypeAlias = VI.Instruction_i

T              = TypeVar('T', bound=AT.ValueCore)

config         = acab.config
FALLBACK_MODAL = config.any_of().symbols.FALLBACK_MODAL()
UUID_CHOP      = config.any_of().print..UUID_CHOP()
BIND_SYMBOL    = config.any_of().symbols.BIND()



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
        match other:
            case str() if other[:2] == "_:":
                # Utility for str comparison. underscore colon signifies sen str
                return str(self) == other[2:]
            case str():
                # If not a sen str, just compare to the name
                return self.key() == other
            case _:
                raise TypeError("Unknown comparison", other)

        if not isinstance(other, VI.Instruction_i):
            # anything not a sentence isn't equal
            return False

        match (len(other), len(other.params)):
            case len(self), len(other.params):
                return all([x == y for x,y in zip(self, other)]) and all([x == y for x,y in zip(self.params, other.params)])
            case _:
                return False


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


class _InstructionVariableTestsImpl(VI.Instruction_i, VP.VariableTests_p):
    @property
    @cache
    def is_var(self) -> bool:
        return False

    @property
    @cache
    def is_at_var(self) -> bool:
        return False

    @property
    @cache
    def has_var(self) -> bool:
        if not bool(self):
            return False
        if self.is_var:
            return True
        return any([x.is_var for x in self.params]) or any([x.has_var for x in self.value])

    @property
    @cache
    def vars(self) -> list[Value_A]:
        return {x for x in self.value if x.is_var} + {x for x in self.params if x.is_var}


class _InstructionCollectionImpl(VI.Instruction_i, Collection):

    def __contains__(self, value) -> bool:
        assert(isinstance(value, (str, VI.Value_i)))
        val = cast(list[VI.Value_i], self.value)
        return value in val

    @cache
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
