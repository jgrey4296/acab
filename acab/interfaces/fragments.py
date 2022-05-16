#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Collection, Final,
                    Generic, Iterable, Iterator, Mapping, Match,
                    MutableMapping, Protocol, Sequence, Tuple, TypeAlias,
                    TypeGuard, TypeVar, cast, final, overload,
                    runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

logging = root_logger.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

import acab.interfaces.handler_system as HS
from acab import types as AT
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.printing import PrintSystem_i
from acab.interfaces.protocols import handler_system as HSubP
from acab.interfaces.protocols.value import AcabReducible_p
from acab.interfaces.semantic import SemanticSystem_i
from acab.interfaces.value import Sentence_i, Value_i, Action_i, Operator_i

Handler_A          : TypeAlias = AT.Handler
ModuleFragment     : TypeAlias = AT.ModuleFragment
HandlerSpec_A      : TypeAlias = AT.HandlerSpec
HandlerSystem_A   : TypeAlias = AT.HandlerSystem

@runtime_checkable
class HandlerFragment_p(Collection["HandlerSpec_A|Handler_A"], Protocol):
    @abc.abstractmethod
    def __bool__(self): pass

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
    specs       : list[HandlerSpec_A]          = field(default_factory=list)
    handlers    : list[Handler_A]              = field(default_factory=list)
    target_i    : None | Type[HandlerSystem_A] = field(default=None, kw_only=True)

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


@runtime_checkable
class UnifiedFragment_p(Protocol):
    def build_dsl(self) -> FI.DSL_Fragment_i: pass
    def build_printers(self) -> FI.Printer_Fragment_i: pass
    def build_semantics(self) -> FI.Semantic_Fragment_i: pass
    def build_operators(self) -> [AT.Sentence]:
        return []
    def build_load_file_paths(self) -> list[str]:
        return []
@dataclass
class ModuleFragment:
    """ Simple holder for extracted module components """

    source        : str                              = field()
    fragments     : InitVar[list[HandlerFragment_i]] = field()
    dsl_fragments : list[HandlerFragment]            = field(default_factory=list)
    semantics     : list[HandlerFragment]            = field(default_factory=list)
    printers      : list[HandlerFragment]            = field(default_factory=list)
    operators     : list[Sentence]                   = field(default_factory=list)

    def __post_init__(self, fragments):
        """
        group the fragments used to build the ModuleFragment,
        flattening UnifiedFragments and ModuleFragments
        """
        remaining = fragments[:]
        while bool(remaining):
            frag = remaining.pop()
            match frag:
                case list() | set():
                    remaining += frag
                case None:
                    continue
                case DSL_Fragment_i():
                    self.dsl_fragments.append(frag)
                case Semantic_Fragment_i():
                    self.semantics.append(frag)
                case Printer_Fragment_i():
                    self.printers.append(frag)
                case Sentence_i():
                    self.operators.append(frag)
                case Action_i() | Operator_i():
                    loose_op_sen = VF.sen() << ["acab", "loose", "operators"] << frag
                    self.operators.append(loose_op_sen)
                case UnifiedFragment_p():
                    remaining.append(frag.build_dsl())
                    remaining.append(frag.build_semantics())
                    remaining.append(frag.build_printers())
                    remaining.extend(frag.build_operators())
                case ModuleFragment():
                    self.dsl_fragments.append(frag.dsl_fragments)
                    self.semantics.append(frag.semantics)
                    self.printers.append(frag.printers)
                    # Again extended
                    self.operators.extend(frag.operators)
                case _:
                    raise TypeError(f"Unrecognised type for module {self.source}: {frag}")

    def __bool__(self):
        return any([bool(x) for x in [self.dsl_fragments, self.semantics, self.printers, self.operators]])

    def __repr__(self) -> str:
        frags     = f"{ len(self.dsl_fragments) } DSL"
        semantics = f"{ len(self.semantics) } Sem"
        printers  = f"{ len(self.printers) } Pr"
        operators = f"{ len(self.operators) } Op"

        return f"({frags} | {semantics} | {operators} | {printers} : {self.source})"

    def report(self) -> str:
        frags     = f"{ len(self.dsl_fragments) } DSL Fragments"
        semantics = f"{ len(self.semantics) } Semantic Components"
        printers  = f"{ len(self.printers) } Printers"
        operators = f"{ len(self.operators) } Operators"

        return f"Module {self.source}:\n- {frags}\n- {semantics}\n- {operators}\n- {printers}"

