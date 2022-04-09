"""
Provide a number of individual interfaces for top level Engine functionality
"""
# pylint: disable=multiple-statements,abstract-method,invalid-sequence-index
from __future__ import annotations

import abc
import collections.abc as cABC
import logging as logmod
import sys
import traceback
from dataclasses import dataclass, field
from types import ModuleType, TracebackType
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Protocol, Sequence, Tuple,
                    TypeAlias, TypeVar, cast)

from acab import types as AT
from acab.error.importer import AcabImportException

logging = logmod.getLogger(__name__)

Sentence           : TypeAlias = AT.Sentence
HandlerFragment    : TypeAlias = AT.HandlerFragment
Operator           : TypeAlias = "AT.Operator[AT.TValCore]"

#----------------------------------------
@dataclass(frozen=True)
class ModuleComponents():
    """ Simple holder for extracted module components """

    source        : str                   = field()
    dsl_fragments : list[HandlerFragment] = field()
    semantics     : list[HandlerFragment] = field()
    printers      : list[HandlerFragment] = field()
    operators     : list[Sentence]         = field()

    def report(self) -> str:
        frags     = f"{ len(self.dsl_fragments) } DSL Fragments"
        semantics = f"{ len(self.semantics) } Semantic Components"
        printers  = f"{ len(self.printers) } Printers"
        operators = f"{ len(self.operators) } Operators"

        return f"Module {self.source}:\n- {frags}\n- {semantics}\n- {operators}\n- {printers}"

    def __repr__(self) -> str:
        frags     = f"{ len(self.dsl_fragments) } DSL"
        semantics = f"{ len(self.semantics) } Sem"
        printers  = f"{ len(self.printers) } Pr"
        operators = f"{ len(self.operators) } Op"

        return f"({frags} | {semantics} | {operators} | {printers} : {self.source})"


#--------------------
class _ModuleLoader_p(Iterable[ModuleComponents], Protocol):
    @abc.abstractmethod
    def __getitem__(self, key:str) -> ModuleComponents: pass
    @abc.abstractmethod
    def __repr__(self) -> str: pass
    @abc.abstractmethod
    def __iter__(self) -> Iterator[ModuleComponents]: pass
    @abc.abstractmethod
    def __len__(self) -> int: pass
    @abc.abstractmethod
    def __contains__(self, other:str) -> bool: pass
    @abc.abstractmethod
    def reload_all_modules(self) -> None: pass
    @abc.abstractmethod
    def load_modules(self, *modules: ModuleType|str) -> list[ModuleComponents]: pass
    @abc.abstractmethod
    def load_module(self, maybe_module: ModuleType | str) -> ModuleComponents: pass
    @abc.abstractmethod
    def extract_from_module(self, module: ModuleType) -> ModuleComponents:
        """
        DFS on a module to retrieve module components (dsl fragments, semantics,
        operators and printers)
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        pass
@dataclass #type:ignore[misc]
class ModuleLoader_i(_ModuleLoader_p):
    """ Describes how an engine loads ACAB/py modules """
    loaded_modules       : dict[str, ModuleComponents]  = field(init=False, default_factory=dict)

