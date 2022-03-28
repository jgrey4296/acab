"""
A Collection of interfaces describing how information in context is collected, constrained,
and grouped for communication between system components
"""
# pylint: disable=multiple-statements,abstract-method
from __future__ import annotations

import abc
import collections.abc as cABC
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Collection, Generic, Hashable,
                    Iterable, Iterator, Literal, Mapping, Match,
                    MutableMapping, Protocol, Sequence, Set, Tuple, TypeAlias,
                    TypeVar, cast, overload, runtime_checkable)
from uuid import UUID

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.interfaces.sub_protocols.value import (AcabFinishable_p,
                                                AcabReducible_p)

# Type declarations:
T = TypeVar('T')
GenFunc             : TypeAlias = AT.fns.GenFunc
CtxSet              : TypeAlias = AT.CtxSet
CtxIns              : TypeAlias = AT.CtxIns
Value               : TypeAlias = "AT.Value[AT.ValueCore]"
Node                : TypeAlias = AT.Node
Sen                 : TypeAlias = AT.Sentence
ProdComp            : TypeAlias = AT.Component
ProductionContainer : TypeAlias = AT.Component
ModuleComponents    : TypeAlias = AT.ModuleComponents

DelayValue = 'UUID | CtxIns | CtxSet | None'

# Interfaces:

@dataclass(frozen=True)
class NamedCtxSet_d:
    """ A Set for storing UUIDs of ctxinsts,
    paired with some data about them
    """

    instruction : ProductionContainer = field()
    uuids       : list[UUID]          = field()
    # TODO instruction state

@dataclass(frozen=True)
class ContextFailState_d:
    """
    Utility dataclass for holding a ctx with information about where it failed
    """
    ctx       : CtxIns      = field()
    query     : Sen         = field()
    failed_on : Value       = field()
    node      : None | Node = field()

class _Constraint_p(Protocol):
    @abc.abstractmethod
    def test(self, node:Node, ctx:CtxIns) -> None: pass

class ContextSet_i(Hashable, Iterable[CtxIns], Protocol):
    _operators : CtxIns

    @abc.abstractmethod
    def fail(self, instance:CtxIns, word:Value, node:Node, query:Sen) -> None: pass
    @abc.abstractmethod
    def push(self, ctxs:CtxIns) -> None: pass
    @abc.abstractmethod
    def pop(self, top:bool=False) -> CtxIns: pass
    @abc.abstractmethod
    def active_list(self, *, clear:bool=False) -> list[CtxIns]: pass
    @abc.abstractmethod
    def failed_list(self) -> list[ContextFailState_d]: pass


@runtime_checkable
class ContextInstance_i(Hashable, Collection[Value], AcabFinishable_p, Protocol):
    @abc.abstractmethod
    def __contains__(self, value: object) -> bool: pass
    @abc.abstractmethod
    def __getitem__(self, value: Value) -> Any: pass
    @abc.abstractmethod
    def bind(self, word:Value, nodes:list[Node]) -> list[CtxIns]: pass
    @abc.abstractmethod
    def bind_dict(self, the_dict:dict[str, Any]) -> CtxIns: pass
    @abc.abstractmethod
    def finish(self) -> Any: pass

@dataclass(frozen=True) #type:ignore[misc]
class Constraint_i(_Constraint_p):
    source         : Value               = field()
    _test_mappings : dict[str, list[AT.fns.TestFunc]] = field(repr=False)

    # Value -> (key, list[Constraint])
    sieve         : ClassVar[list[GenFunc]]

