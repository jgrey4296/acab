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
from acab.interfaces.fragments import ModuleFragment

logging = logmod.getLogger(__name__)

Sentence           : TypeAlias = AT.Sentence
HandlerFragment    : TypeAlias = AT.HandlerFragment
Operator           : TypeAlias = "AT.Operator[AT.TValCore]"

#--------------------
class _ModuleLoader_p(Iterable[ModuleFragment], Protocol):
    @abc.abstractmethod
    def __getitem__(self, key:str) -> ModuleFragment: pass
    @abc.abstractmethod
    def __repr__(self) -> str: pass
    @abc.abstractmethod
    def __iter__(self) -> Iterator[ModuleFragment]: pass
    @abc.abstractmethod
    def __len__(self) -> int: pass
    @abc.abstractmethod
    def __contains__(self, other:str) -> bool: pass
    @abc.abstractmethod
    def reload_all_modules(self) -> None: pass
    @abc.abstractmethod
    def load_modules(self, *modules: ModuleType|str) -> list[ModuleFragment]: pass
    @abc.abstractmethod
    def load_module(self, maybe_module: ModuleType | str) -> ModuleFragment: pass
    @abc.abstractmethod
    def extract_from_module(self, module: ModuleType) -> ModuleFragment:
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
    loaded_modules       : dict[str, ModuleFragment]  = field(init=False, default_factory=dict)
