#!/opts/anaconda3/envs/ENV/python
# pylint: disable=multiple-statements,abstract-method,invalid-sequence-index
from __future__ import annotations
import abc
import collections.abc as cABC
import logging as logmod
from dataclasses import InitVar, dataclass, field, replace
from enum import Enum, EnumMeta
from types import MethodType
from typing import (Any, Callable, ClassVar, Collection, Container, Final,
                    Generic, Iterable, Iterator, Mapping, Match,
                    MutableMapping, NewType, Protocol, Sequence, Tuple, Type,
                    TypeAlias, TypeVar, cast, final, overload,
                    runtime_checkable)

from acab import types as AT
from acab import AcabConfig
from acab.interfaces.sieve import AcabSieve
from acab.interfaces.protocols import handler_system as HSubP

logging = logmod.getLogger(__name__)
config  = AcabConfig()

GenFunc               : TypeAlias = AT.fns.GenFunc
TypeFunc              : TypeAlias = AT.fns.TypeFunc
ModuleComponents      : TypeAlias = AT.ModuleComponents
Overrider             : TypeAlias = AT.HandlerOverride
Sen_A                 : TypeAlias = AT.Sentence
Structure             : TypeAlias = "AT.DataStructure[AT.Node]"
Instruction_A         : TypeAlias = AT.Instruction
Value                 : TypeAlias = "AT.Value[AT.ValueCore]"
Handler_A             : TypeAlias = AT.Handler
HandlerSpec_A         : TypeAlias = AT.HandlerSpec
HandlerComponent_A    : TypeAlias = AT.HandlerComponent
Handler_System_A      : TypeAlias = AT.HandlerSystem
HandlerFragment_A     : TypeAlias = AT.HandlerFragment
RegistrationTargets_A : TypeAlias = "HandlerFragment_A|HandlerSpec_A|Handler_A|AT.HandlerOverride|dict[str,Any]"

HandlerFlags          : EnumMeta = Enum("HandlerFlags", "OVERRIDE MERGE APPEND PREPEND COLLECT REDUCE")

HandlerFlags_t        : TypeAlias = Type[HandlerFlags]

@dataclass #type:ignore[misc]
class HandlerSystem_i(HSubP.HandlerSystem_p):
    init_specs     : InitVar[list[HandlerSpec_A]] = None
    init_handlers  : InitVar[list[Handler_A]]     = None
    # TODO make default  Tuple[str, str], and lookup?
    sieve_fns      : InitVar[list[GenFunc]]       = None

    sieve          : AcabSieve[str]               = field(init=False, default_factory=AcabSieve)
    handler_specs  : dict[str, HandlerSpec_A]     = field(init=False, default_factory=dict)
    loose_handlers : list[Handler_A]              = field(init=False, default_factory=list)
    _data          : dict[str, Any]               = field(init=False, default_factory=dict)

    _default_sieve : ClassVar[list[GenFunc]]      = [str]

    def __post_init__(self, init_specs:list[HandlerSpec_A], init_handlers:list[Handler_A], sieve_fns:list[GenFunc]) -> None: pass

@dataclass #type:ignore[misc]
class HandlerSpec_i(HSubP.HandlerSpec_p):
    signal          : 'str | Sen_A'                      = field()
    flags           : list[Enum]                         = field(default_factory=list)
    func_api        : None | Type[Any] | TypeFunc        = field(default=None)
    struct_api      : None | Type[Any] | Type[Structure] = field(default=None)
    data_api        : list[str]                          = field(default_factory=list)
    handler_limit   : None | slice                       = field(default=None)

    data            : dict[str, Any]                     = field(init=False, default_factory=dict)
    handlers        : list[Handler_A]                    = field(init=False, default_factory=list)
    struct          : None | Structure                   = field(init=False, default=None)
    h_limit_history : bool                               = field(init=False, default=False)

    flag_e          : ClassVar[HandlerFlags_t]       = HandlerFlags


@dataclass #type:ignore[misc]
class HandlerComponent_i(HSubP.HandlerComponent_p):
    """ Utility Class Component for easy creation of a handler """
    signal : None | str = field(default=None)

@dataclass #type:ignore[misc]
class Handler_i(HSubP.Handler_p):
    """ A Handler implementation for registering
    individual functions or methods """
    signal   : Sen_A | str
    func     : None | HandlerComponent_A | GenFunc | type        = field(default=None, kw_only=True)
    struct_i : None | type[Structure] | Callable[..., Structure] = field(default=None, kw_only=True)
    verify_f : None | GenFunc                                    = field(default=None, kw_only=True)
    flags    : set[Enum]                                         = field(default_factory=set, kw_only=True)

    struct   : None | Structure                                  = field(default=None, kw_only=True)

    def __post_init__(self) -> None: pass

@dataclass #type:ignore[misc]
class HandlerOverride:
    """ Simple Wrapper for forced semantic use
        ie: a continuation
        """
    signal   : str              = field()
    value    : Value            = field()
    data     : dict[Any, Any]   = field(default_factory=dict)

    def replace(self, *, signal=None, data=None):
        updated = replace(self,
                          signal=signal or self.signal)
        updated.data.update(data)
        return updated
