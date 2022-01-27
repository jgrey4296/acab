#!/usr/bin/env python3
from uuid import UUID
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from enum import Enum
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab import types as AT
# Type declarations:
CtxSet     = AT.CtxSet
CtxIns     = AT.CtxIns
Value      = AT.Value
ProdComp   = AT.Component
DelayValue = Union[UUID, CtxIns, CtxSet, None]


@dataclass
class DelayedCommands_i():

    delayed_e: Enum                    = field()
    _purgatory : Dict[Enum, Set[UUID]] = field(init=False, default_factory=dict)
    _priority : List[Enum]             = field(init=False, default_factory=list)

    def delay(self, instr:Enum, *, ctxIns:DelayValue):
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
        logging.debug(f"Performing {len(self._purgatory)} Delayed Instructions")
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
