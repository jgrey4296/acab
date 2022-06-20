"""
A Collection of interfaces describing how information in context is collected, constrained,
and grouped for communication between system components
"""
# pylint: disable=multiple-statements,abstract-method
from __future__ import annotations

import abc
import collections.abc as cABC
import logging as logmod
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Collection, Generic, Hashable,
                    Iterable, Iterator, Literal, Mapping, Match,
                    MutableMapping, Protocol, Sequence, Set, Tuple, TypeAlias,
                    TypeVar, cast, overload, runtime_checkable, ContextManager)
from uuid import UUID

logging = logmod.getLogger(__name__)

from acab import types as AT
from acab.interfaces.protocols.value import AcabFinishable_p

# Type declarations:
T = TypeVar('T')
GenFunc             : TypeAlias = AT.fns.GenFunc
CtxSet              : TypeAlias = AT.CtxSet
CtxIns              : TypeAlias = AT.CtxIns
Value               : TypeAlias = "AT.Value[AT.ValueCore]"
View                : TypeAlias = AT.StructView
Sen                 : TypeAlias = AT.Sentence
ProductionContainer : TypeAlias = AT.Container
ModuleFragment      : TypeAlias = AT.ModuleFragment

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
    node      : None | View = field()

class _Constraint_p(Protocol):
    @abc.abstractmethod
    def test(self, node:View, ctx:CtxIns) -> None: pass

@runtime_checkable
class ContextSet_p(Hashable, Iterable[CtxIns], Protocol):

    @abc.abstractmethod
    def fail(self, instance:CtxIns, word:Value, node:View, query:Sen) -> None: pass
    @abc.abstractmethod
    def push(self, ctxs:CtxIns) -> None: pass
    @abc.abstractmethod
    def pop(self, top:bool=False) -> CtxIns: pass
    @abc.abstractmethod
    def active_list(self, *, clear:bool=False) -> list[CtxIns]: pass
    @abc.abstractmethod
    def failed_list(self) -> list[ContextFailState_d]: pass

class ContextSet_i(ContextSet_p):
    _operators : CtxIns

@runtime_checkable
class ContextInstance_i(Hashable, Collection[Value], AcabFinishable_p, Protocol):
    @abc.abstractmethod
    def __contains__(self, value: object) -> bool: pass
    @abc.abstractmethod
    def __getitem__(self, value: Value) -> Any: pass
    @abc.abstractmethod
    def progress(self, word:Value|dict[str, Any], nodes:list[View]|dict[str|View], sub_binds=None) -> list[CtxIns]: pass
    @abc.abstractmethod
    def finish(self) -> Any: pass
    @property
    @abc.abstractmethod
    def current_node(self) -> View: pass
@dataclass(frozen=True) #type:ignore[misc]
class Constraint_i(_Constraint_p):
    source         : Value               = field()
    _test_mappings : dict[str, list[AT.fns.TestFunc]] = field(repr=False)

    # Value -> (key, list[Constraint])
    sieve         : ClassVar[list[GenFunc]]



@dataclass
class CtxManager_i(ContextManager):

    target_clause : None|Sen = field()
    root_node     : View     = field()
    ctxs          : CtxSet   = field()

    _purgatory : list[ContextInstance_i] = field(init=False, default_factory=list)

    def __iter__(self): pass
    def __enter__(self): pass
    # Must call activate_ctxs
    def __exit__(self, exc_type, exc_value, traceback): pass

    # Getting the current state
    @property
    def finished(self):
        assert(not bool(self._purgatory))
        return self.ctxs

    @property
    def current(self) -> Iterator[Value]: pass
    @property
    def active(self) -> Iterator[Tuple[Value, CtxIns, View]]: pass

    def maybe_test(self, results:list[View]) -> list[View]: pass
    # For recording results of tests
    def queue_ctxs(self, ctxs:list[ContextInstance_i]):
        self._purgatory += ctxs

    def activate_ctxs(self):
        self.ctxs.push(self._purgatory[:])
        self._purgatory = []

    def collect(self):
        """
        Context collecton specific vars.
        Flattens many contexts into one, with specified variables
        now as lists accumulated from across the contexts.

        Semantics of collect:
        0[ctxs]0 -> fail
        1[ctxs]n -> 1[α]1
        where
        α : ctx = ctxs[0] ∪ { β : ctx[β] for ctx in ctxs[1:] }
        β : var to collect


        """
        if not bool(self.collect_vars):
            return

        # select instances with bindings
        # Merge into single new instance
        # replace
        raise NotImplementedError()

