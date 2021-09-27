"""
A Collection of interfaces describing how information in context is collected, constrained,
and grouped for communication between system components
"""

import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from enum import Enum
from uuid import UUID

logging = root_logger.getLogger(__name__)

from acab import types as AT

# Type declarations:
CtxSet      = AT.CtxSet
CtxIns      = AT.CtxIns
DelayValue  = Union[UUID, CtxIns, CtxSet, None]


# Interfaces:
@dataclass
class Constraint_i(metaclass=abc.ABCMeta):
    @staticmethod
    def build(word, operators):
        pass

    @abc.abstractmethod
    def test_all(self, node, ctx):
        pass

@dataclass
class ContextSet_i(metaclass=abc.ABCMeta):

    @staticmethod
    def build(ops):
        pass

    @abc.abstractmethod
    def fail(self, instance, word, node):
        pass

    @abc.abstractmethod
    def test(self, ctx, possible, word):
        pass

    @abc.abstractmethod
    def push(self, ctxs):
        pass

    @abc.abstractmethod
    def pop(self, top=False):
        pass

    @abc.abstractmethod
    def __len__(self):
        pass
    @abc.abstractmethod
    def __hash__(self):
        pass
    @abc.abstractmethod
    def active_list(self, clear=False):
        pass
    @abc.abstractmethod
    def failed_list(self):
        pass
    @abc.abstractmethod
    def __getitem__(self, index):
        pass
@dataclass
class ContextInstance_i(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def bind(self, word, nodes):
        pass

    @abc.abstractmethod
    def bind_dict(self, the_dict):
        pass

    @abc.abstractmethod
    def __contains__(self, value):
        pass

    @abc.abstractmethod
    def __getitem__(self, value):
        pass

    @abc.abstractmethod
    def to_sentences(self):
        """ Convert to sentences for printing """
        pass

    @abc.abstractmethod
    def __len__(self):
        pass


@dataclass
class DelayedCommands_i(metaclass=abc.ABCMeta):

    delayed_e: Enum                    = field()
    _purgatory : Dict[Enum, Set[UUID]] = field(init=False, default_factory=dict)
    _priority : List[Enum]             = field(init=False, default_factory=list)

    def delay(self, instr:Enum, ctxIns:DelayValue):
        """
        Register an action for later.
        Useful for adding ctxins results without interfering with current operations,
        """
        assert(isinstance(instr, self.delayed_e))
        if instr not in self._purgatory:
            self._purgatory[instr] = set()

        if ctxIns is None:
            return

        self._purgatory[instr].add(ctxIns)

    def run_delayed(self):
        """ Similar to Cmd implementation, each instr should have a do_{x} method """
        logging.debug("Running Delayed Instructions")
        # run priority enums
        # if self.delayed_e.CLEAR in self._purgatory:
        #     self._active = []
        #     del self._purgatory[self.delayed_e.CLEAR]

        for instr in self._purgatory.keys():
            method_name = f"do_{instr.name.lower()}"
            if hasattr(self, method_name):
                getattr(self, method_name)(self._purgatory[instr])
            else:
                self.do_default(instr, self._purgatory[instr])

        self._purgatory = {}
