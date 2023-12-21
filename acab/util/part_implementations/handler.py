#!/usr/bin/env python3
"""
This is separated from the rest of the handler_system to avoid a circular import

"""
# pylint: disable=abstract-method,invalid-sequence-index,use-a-generator,too-many-lines
# pyright: reportPrivateUsage=warning
##-- imports
from __future__ import annotations

import logging as logmod
from dataclasses import InitVar, dataclass, field, replace
from enum import Enum
from types import MethodType
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, NewType,
                    Protocol, Sequence, Tuple, Type, TypeAlias, TypeVar, cast)

import acab
from acab.core.util.decorators.util import cache
from acab.error.handler import AcabHandlerException
from acab.interfaces import handler_system as HS
from acab.interfaces.config import ConfigSpec_d
from acab.interfaces.data import Structure_i
from acab.interfaces.protocols import handler_system as HSubP
from acab.interfaces.sieve import AcabSieve
from acab.interfaces.value import Sentence_i, Value_i
from acab_config import AcabProtocolError as APE

if TYPE_CHECKING:
    # tc only imports
    from acab import types as AT
    ModuleFragment     : TypeAlias = AT.ModuleFragment
    Overrider          : TypeAlias = AT.HandlerOverride
    Sen_A              : TypeAlias = AT.Sentence
    Structure          : TypeAlias = "AT.DataStructure[AT.Node]"
    Value              : TypeAlias = "AT.Value[AT.ValueCore]"
    Handler_A          : TypeAlias = AT.Handler
    HandlerSpec_A      : TypeAlias = AT.HandlerSpec
    HandlerComponent_A : TypeAlias = AT.HandlerComponent
    Handler_System_A   : TypeAlias = AT.HandlerSystem

##-- end imports

logging     = logmod.getLogger(__name__)
config      = acab.config

@APE.assert_concrete
@dataclass
class BasicHandler(HS.Handler_i):

    def __post_init__(self) -> None:
        if isinstance(self.func, type):
            self.func = self.func()

        if self.func is not None:
            assert(callable(self.func))

        if self.struct is not None or self.struct_i is None:
            pass
        elif hasattr(self.struct_i, "build_default"):
            self.struct = self.struct_i.build_default() #type:ignore
        else:
            self.struct = self.struct_i() #type:ignore

    def __call__(self, *args, **kwargs):
        if self.func is None:
            raise AcabHandlerException("Attempt to Call Struct Handler", rest=[self])
        return self.func(*args, **kwargs)

    def __iter__(self):
        """ unpack the handler"""
        return (self.func, self.struct).__iter__()

    def __repr__(self):
        sig_s       = str(self.signal)
        func_name   = ""
        struct_name = ""
        if self.func is not None:
            func_name = str(self.func.__class__.__name__) #type:ignore
        if self.struct is not None:
            struct_name = str(self.struct.__class__.__name__)

        return f"<{self.__class__.__name__}({sig_s}: {func_name}: {struct_name})>"

    def verify(self, instruction):
        result = False
        if self.verify_f is not None:
            result = self.verify_f(instruction)
        else:
            result = True

        if not result and self.func is not None and hasattr(self.func, "verify"):
            result = result and self.func.verify(instruction) #type:ignore

        return result

    def as_handler(self, *, signal=None, struct=None, flags=None):
        """
        Duplicate the handler, with some changes
        """
        return self.__class__(signal or self.signal,
                              func=self.func,
                              struct=struct or self.struct,
                              flags=flags or self.flags)

    @cache
    def __str__(self):
        return str(self.signal)
