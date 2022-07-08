"""

"""
# pylint: disable=multiple-statements,protected-access,too-many-ancestors,abstract-method
# pyright: reportGeneralTypeIssues=warning
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, Type, TypeAlias, TypeGuard,
                    TypeVar, cast, final, overload, runtime_checkable)

from acab import AcabConfig
from acab import types as AT
from acab import AcabConfig
from acab.core.defaults.print_symbols import PRINT_SEPARATOR_P
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces import handler_system as HS
from acab.interfaces.protocols import handler_system as HSubP

logging = logmod.getLogger(__name__)
config = AcabConfig()

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    # tc only imports
    Config_A         : TypeAlias = AT.Config
    Value_A          : TypeAlias = "AT.Value[AT.ValueCore]"
    Sentence         : TypeAlias = AT.Sentence
    ModuleFragment   : TypeAlias = AT.ModuleFragment
    ConfigSpec       : TypeAlias = AT.ConfigSpec
    GenFunc          : TypeAlias = AT.fns.GenFunc
    Handler_A        : TypeAlias = AT.Handler
    HandlerSpec_A    : TypeAlias = AT.HandlerSpec

# Protocols  ##################################################################
@runtime_checkable
class _PrintSystem_p(HSubP.HandlerSystem_p, Protocol):
    @abc.abstractmethod
    def __call__(self, *args:Sentence) -> str: pass
    @abc.abstractmethod
    def check(self, val:str) -> None | str: pass
    @abc.abstractmethod
    def pprint(self, *args:Sentence) -> str: pass


@runtime_checkable
class _PrintSemantics_p(Protocol):

    @abc.abstractmethod
    def add_transforms(self) -> list[GenFunc]: pass
    @abc.abstractmethod
    def run_transforms(self, value: Value_A, curr_str: list[Any]) -> list[Any]: pass
    @abc.abstractmethod
    def __call__(self, to_print:Value_A, *, top:None|PrintSystem_i=None, data:None|dict[str,Any]=None) -> list[str | Value_A]: pass


# Interfaces  #################################################################
@dataclass #type:ignore[misc]
class PrintSystem_i(HS.HandlerSystem_i, _PrintSystem_p):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures,
    just consumes Sentences
    """
    init_specs     : InitVar[list[HandlerSpec_A]] = None
    init_handlers  : InitVar[list[Handler_A]]     = None
    sieve_fns      : InitVar[list[GenFunc]]       = None
    separator : ConfigSpec     = field(default=PRINT_SEPARATOR_P)
    settings  : dict[str, str] = field(default_factory=dict)
    _config   : Config_A       = field(init=False, default_factory=AcabConfig)

    # pyrite bug : doesn't handle inherited initvars
    # pylint: disable-next=arguments-renamed
    def __post_init__(self, specs:list[HandlerSpec_A], handlers:list[Handler_A], sieve_fns:list[GenFunc]) -> None: pass

@dataclass #type:ignore[misc]
class PrintSemantics_i(HS.HandlerComponent_i, _PrintSemantics_p):
    transforms  : list[GenFunc] = field(init=False, default_factory=list)

    def __post_init__(self) -> None: pass

