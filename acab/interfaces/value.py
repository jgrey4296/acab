"""
Interfaces for the use of actual information, both individual,
and formed into sentences
"""
import abc
import collections.abc as cABC
import logging as root_logger
from dataclasses import dataclass, field
from typing import (Any, Mapping, Match, MutableMapping, Protocol,
                    Sequence, Type, Tuple, TypeVar, cast, TypeAlias, ClassVar)
from uuid import UUID, uuid1
from re import Pattern
from functools import reduce

from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.interfaces.util import AcabReducible, AcabBuildable, AcabUUID

logging       = root_logger.getLogger(__name__)

config        = AcabConfig.Get()

Sentence    : TypeAlias = AT.Sentence
Value       : TypeAlias = AT.Value
Variable    : TypeAlias = Value
Tag         : TypeAlias = Value
Instruction : TypeAlias = AT.Instruction
ValueData   : TypeAlias = str
# extended in AcabValue to also have Value_i:
ValueCore   = str | Pattern | list | None

# Data ########################################################################
@dataclass(frozen=True)
class _Value_d:
    name   : None | str                  = field(default=None)
    value  : ValueCore            = field(default=None)
    params : list[Variable]       = field(default_factory=list)
    tags   : set[Tag]             = field(default_factory=set)
    data   : dict[ValueData, Any] = field(default_factory=dict)
    uuid   : UUID                 = field(default_factory=uuid1)


@dataclass(frozen=True)
class _Instruction_d(_Value_d):

    breakpoint : bool = field(init=False, default=False)
    # TODO add listener field for similar to breakpoint


@dataclass(frozen=True)
class _Sentence_d(_Instruction_d):

    value: list[Value]  = field(default_factory=list)
    # TODO a weak dict mapping names -> values

# Protocols ###################################################################
class Value_i(cABC.Hashable, AcabBuildable, _Value_d):

    @staticmethod
    def extend_core(*ts:Type):
        ValueCore = reduce(lambda a, b: a | b, ts, ValueCore)

    @property
    @abc.abstractmethod
    def type(self) -> Sentence:
        pass

    @abc.abstractmethod
    def copy(self, **kwargs) -> Value:
        pass

    @abc.abstractmethod
    def apply_params(self, params, *, data=None) -> Value:
        pass

    @abc.abstractmethod
    def apply_tags(self, tags, *, data=None) -> Value:
        pass


    @property
    @abc.abstractmethod
    def is_var(self) -> bool:
        pass

    @property
    @abc.abstractmethod
    def is_at_var(self) -> bool:
        pass


    @property
    @abc.abstractmethod
    def has_var(self) -> bool:
        pass

    @abc.abstractmethod
    def key(self) -> str:
        pass



class Instruction_i(Value_i, cABC.Sized, cABC.Container, AcabReducible, _Instruction_d):

    def do_break(self):
        self.breakpoint = not self.breakpoint

    @property
    def should_break(self) -> bool:
        return self.breakpoint


class Sentence_i(Instruction_i, cABC.Sequence, _Sentence_d):

    @abc.abstractmethod
    def build(words, **kwargs):
        pass

    @abc.abstractmethod
    def attach_statement(self, value: Value) -> Sentence:
        pass

    @abc.abstractmethod
    def detach_statement(self) -> Sentence:
        pass

    @abc.abstractmethod
    def __len__(self):
        pass
    @abc.abstractmethod
    def __iter__(self):
        pass
    @abc.abstractmethod
    def __getitem__(self, i):
        pass

    @property
    def words(self) -> list[Value]:
        return self.value[:]



class Operator_i(Value_i):
    pass


class ActionOperator(Operator_i):
    # Needs to be concrete not a NewType,
    # as its used in isinstance checks
    pass

