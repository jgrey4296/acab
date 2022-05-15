#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from re import Pattern
from typing import (TYPE_CHECKING, Any, Collection, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

logging = root_logger.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

import acab.interfaces.handler_system as HS
from acab import types as AT
from acab.core.value.default_structure import QUERY
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.protocols import handler_system as HSubP
from acab.interfaces.protocols.value import AcabReducible_p
from acab.interfaces.value import Sentence_i, Value_i
from acab.interfaces.printing import PrintSystem_i
from acab.interfaces.semantic import SemanticSystem_i

Value              : TypeAlias = "AT.Value[AT.ValueCore]"
Sen_A              : TypeAlias = AT.Sentence
Instruction        : TypeAlias = AT.Instruction
Struct_A           : TypeAlias = "AT.DataStructure[AT.Node]"
Node               : TypeAlias = AT.Node
Engine             : TypeAlias = AT.Engine
CtxSet             : TypeAlias = AT.CtxSet
CtxIns             : TypeAlias = AT.CtxIns
Handler_A          : TypeAlias = AT.Handler
ProductionOperator : TypeAlias = "AT.Operator[AT.TValCore]"
ModuleComponents   : TypeAlias = AT.ModuleComponents
StructureSemantics : TypeAlias = AT.StructureSemantics
ValueSemantics     : TypeAlias = AT.ValueSemantics
StatementSemantics : TypeAlias = AT.StatementSemantics
SemanticSystem     : TypeAlias = AT.SemanticSystem
PrintSystem        : TypeAlias = AT.PrintSystem


@runtime_checkable
class HandlerFragment_p(Collection["HandlerSpec_A|Handler_A"], Protocol):
    # TODO just make this a newtype?

    @abc.abstractmethod
    def __contains__(self, other): pass

    @abc.abstractmethod
    def __iter__(self): pass

    @abc.abstractmethod
    def __len__(self): pass

@dataclass #type:ignore[misc]
class HandlerFragment_i(HandlerFragment_p):
    """ Structure of Handlers to be added to a system, and any
    data they require
    """
    specs       : list[HandlerSpec_A]           = field(default_factory=list)
    handlers    : list[Handler_A]               = field(default_factory=list)
    target_i    : None | Type[Handler_System_A] = field(default=None, kw_only=True)

class DSL_Fragment_i(HandlerFragment_i):
    # TODO maybe a newtype
    pass

@dataclass #type:ignore[misc]
class Semantic_Fragment_i(HandlerFragment_i):
    """ Dataclass of Semantic Handlers to be added to the system, and any
    data they require
    """
    target_i : None | Type[SemanticSystem] = field(default=SemanticSystem_i) #type:ignore[assignment]

@dataclass #type:ignore[misc]
class Printer_Fragment_i(HandlerFragment_i):
    target_i : Type[PrintSystem] = field(kw_only=True, default=PrintSystem_i)
