# pylint: disable=abstract-method,invalid-sequence-index,use-a-generator,too-many-lines
# pyright: reportPrivateUsage=warning
from __future__ import annotations
import logging as logmod
from dataclasses import InitVar, dataclass, field, replace
from enum import Enum
from types import MethodType
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, NewType, Protocol,
                    Sequence, Tuple, Type, TypeAlias, TypeVar, cast)

from acab import types as AT
from acab import AcabConfig
from acab.core.util.decorators.util import cache
from acab.error.handler import AcabHandlerException
from acab.interfaces import handler_system as HS
from acab.interfaces.config import ConfigSpec_d
from acab.interfaces.data import Structure_i
from acab.interfaces.sieve import AcabSieve
from acab.interfaces.value import Sentence_i, Value_i
from acab.error.protocol import AcabProtocolError as APE
from acab.interfaces.protocols import handler_system as HSubP
from acab.core.util.part_implementations.handler import Handler

logging = logmod.getLogger(__name__)

config = AcabConfig()
SPACER = int(config.prepare("Print.Data", "SPACER_SIZE")())
DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

ModuleComponents   : TypeAlias = AT.ModuleComponents
Overrider          : TypeAlias = AT.HandlerOverride
Sen_A              : TypeAlias = AT.Sentence
Structure          : TypeAlias = "AT.DataStructure[AT.Node]"
Value              : TypeAlias = "AT.Value[AT.ValueCore]"
Handler_A          : TypeAlias = AT.Handler
HandlerSpec_A      : TypeAlias = AT.HandlerSpec
HandlerComponent_A : TypeAlias = AT.HandlerComponent
Handler_System_A   : TypeAlias = AT.HandlerSystem

PASSTHROUGH         = "_"


@APE.assert_concrete
class PatchHandler(Handler, HS.Handler_i):
    """
    A Monkey-Patching handler to cut down on stack frames
    https://stackoverflow.com/questions/38541015/
    """

    def __post_init__(self) -> None:
        if isinstance(self.func, type):
            self.func = self.func()

        if self.func is not None:
            self.__patch_call()

        if self.struct is not None or self.struct_i is None:
            pass
        elif hasattr(self.struct_i, "build_default"):
            self.struct = self.struct_i.build_default() #type:ignore
        else:
            self.struct = self.struct_i() #type:ignore

    def __patch_call(self):
        assert(callable(self.func))
        class PatchedHandler(type(self)):
            __call__ = self.func.__call__

        self.__class__ = PatchedHandler

    def __call__(self, *args, **kwargs):
        raise AcabHandlerException("Attempt to Call PatchHandler", rest=[self])
