"""
A Collection of interfaces describing how information in context is collected, constrained,
and grouped for communication between system components
"""

import abc
import collections.abc as cABC
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Protocol, Sequence, Tuple,
                    TypeAlias, TypeVar, cast)
from uuid import UUID

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.interfaces.util import AcabFinishable, AcabReducible

# Type declarations:
CtxSet   : TypeAlias = AT.CtxSet
CtxIns   : TypeAlias = AT.CtxIns
Value    : TypeAlias = AT.Value
ProdComp : TypeAlias = AT.Component

DelayValue = UUID | CtxIns | CtxSet | None


# Interfaces:
@dataclass(frozen=True)
class _Constraint_d:
    source         : Value                     = field()
    _test_mappings : dict[str, list[Callable]] = field(repr=False)

    # Value -> (key, list[Constraint])
    sieve         : ClassVar[list[Callable]]

class Constraint_i(_Constraint_d):

    @staticmethod
    def build(word, operators, *, sieve=None):
        pass

    @abc.abstractmethod
    def test(self, node, ctx):
        pass

class ContextSet_i(cABC.Hashable, cABC.Set):

    @staticmethod
    def build(ops):
        pass

    @abc.abstractmethod
    def fail(self, instance, word, node):
        pass

    @abc.abstractmethod
    def push(self, ctxs):
        pass

    @abc.abstractmethod
    def pop(self, top=False):
        pass

    @abc.abstractmethod
    def active_list(self, *, clear=False):
        pass
    @abc.abstractmethod
    def failed_list(self):
        pass

class ContextInstance_i(cABC.Mapping, cABC.Hashable, AcabFinishable):

    @abc.abstractmethod
    def bind(self, word, nodes):
        pass

    @abc.abstractmethod
    def bind_dict(self, the_dict):
        pass

    @abc.abstractmethod
    def finish(self) -> CtxIns:
        """
        TODO rename this
        serves to return a new ctxins with a
        mutable's bindings incorporated
        """
        pass

