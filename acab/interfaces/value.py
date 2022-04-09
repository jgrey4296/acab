"""
Interfaces for the use of actual information, both individual,
and formed into sentences
"""
# pylint: disable=multiple-statements,abstract-method,too-many-ancestors,invalid-sequence-index
from __future__ import annotations
import abc
import collections.abc as cABC
import logging as logmod
from dataclasses import dataclass, field
from functools import reduce
from re import Pattern
from typing import (Any, ClassVar, Container, Final, Generic, Literal, Mapping,
                    Match, MutableMapping, Protocol, Sequence, Sized, Tuple, Collection,
                    Type, TypeAlias, TypeVar, cast, runtime_checkable)
from uuid import UUID, uuid1

from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.error.config import AcabConfigException
import acab.interfaces.protocols.value as VSubP
import acab.core.data.default_structure as DS

__all__ = ['Value_i', 'Instruction_i', 'Sentence_i', 'Operator_i', 'Action_i']

logging       = logmod.getLogger(__name__)

config        = AcabConfig()

GenFunc     : TypeAlias = AT.fns.GenFunc
Sen_A       : TypeAlias = AT.Sentence
Sen_t       : TypeAlias = Type[Sen_A]
Value_A     : TypeAlias = "AT.Value[AT.ValueCore_t]"
Value_t     : TypeAlias = Type[Value_A]
Variable    : TypeAlias = Value_A
Tag         : TypeAlias = "AT.Value[str]"
Instruction : TypeAlias = AT.Instruction
ValueData   : TypeAlias = str
# extended in AcabValue to also have Value_p:

SemSys      : TypeAlias = AT.SemanticSystem

T     = TypeVar('T', bound=AT.ValueCore_t, covariant=True)
T_Cov = TypeVar('T_Cov', covariant=True)

# Data ########################################################################
@dataclass(frozen=True) #type:ignore[misc]
class Value_i(VSubP.Value_p, Generic[T]):
    value  : T                    = field()
    name   : str                  = field()
    params : list[Variable]       = field(default_factory=list)
    tags   : set[Tag]             = field(default_factory=set)
    data   : dict[ValueData, Any] = field(default_factory=dict)
    uuid   : UUID                 = field(default_factory=uuid1)


    _defaults : ClassVar[dict[str,Any]] = {}

    @staticmethod
    def extend_core(*ts:Any) -> None:
        """
        Adds a type to the accepted types Value_p can build
        """
        expansion : Any = reduce(lambda a, b: a | b, ts, AT.ValueCore)
        AT.ValueCore    = expansion

        expansion_t : Any = reduce(lambda a, b: a | b, ts, AT.ValueCore_t)
        TValCore          = TypeVar('TValCore', bound=expansion_t)
        AT.TValCore       = TValCore #type:ignore


    @classmethod
    def _preprocess(cls, value):
        return value



@dataclass(frozen=True) #type:ignore[misc]
class Instruction_i(Value_i[list[Any]], VSubP.Instruction_p):
    breakpoint : bool        = field(init=False, default=False)
    # TODO add listener field for similar to breakpoint


@dataclass(frozen=True) #type:ignore[misc]
class Sentence_i(Instruction_i, VSubP.Sentence_p):
    @property
    def words(self) -> list[Value_A]:
        return self.value[:] #type:ignore

# Combined Interfaces:
class Operator_i(Value_i[None], Generic[T_Cov]):
    def is_operator(self) -> Literal[True]: return True
    @abc.abstractmethod
    def __call__(self, *params: Value_A, data:None|dict[str,Any]=None) -> T_Cov: pass

class Action_i(Value_i[None]):
    def is_action(self) -> Literal[True] : return True
    @abc.abstractmethod
    def __call__(self, *params: Value_A, data:None|dict[str,Any]=None, semsys:None|SemSys=None) -> None: pass


# Factory
class ValueFactory_i(Protocol):
    """ Utility Class for building values
    Must initialize value_fn and sen_fn before using acab
    """
    value_fn : ClassVar[GenFunc|Value_t] = lambda *args, **kwargs: AcabConfigException("ValueFactories needs value_fn to be set")
    sen_fn   : ClassVar[GenFunc|Sen_t]   = lambda *args, **kwargs: AcabConfigException("ValueFactories needs sen_fn to be set")

    @staticmethod
    def value(*args:Any, **kwargs:Any) -> Value_t | GenFunc:
        return ValueFactory_i.value_fn(*args, **kwargs) #type:ignore

    @staticmethod
    def sen(*args:Any, **kwargs:Any) -> Sen_t | GenFunc:
        return ValueFactory_i.sen_fn(*args, **kwargs) #type:ignore

    @staticmethod
    def set(val_fn:Value_t, sen_fn:Sen_t) -> None:
        logging.info("Setting Factory basics to: %s and %s", val_fn, sen_fn)
        ValueFactory_i.value_fn = val_fn
        ValueFactory_i.sen_fn   = sen_fn
