"""
A Collection of interfaces describing how information in context is collected, constrained,
and grouped for communication between system components
"""

import abc
import collections.abc as cABC
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from enum import Enum
from uuid import UUID

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.interfaces.util import AcabReducible


# Type declarations:
CtxSet     = AT.CtxSet
CtxIns     = AT.CtxIns
Value      = AT.Value
ProdComp   = AT.Component
DelayValue = Union[UUID, CtxIns, CtxSet, None]


# Interfaces:
@dataclass(frozen=True)
class Constraint_i(metaclass=abc.ABCMeta):
    source         : Value                     = field()
    _test_mappings : Dict[str, List[Callable]] = field()

    # Value -> (key, List[Constraint])
    sieve         : ClassVar[List[Callable]]

    @staticmethod
    def build(word, operators, *, sieve=None):
        pass

    @abc.abstractmethod
    def test(self, node, ctx):
        pass

@dataclass
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

@dataclass(frozen=True)
class ContextInstance_i(cABC.Mapping, cABC.Hashable, AcabReducible):

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
