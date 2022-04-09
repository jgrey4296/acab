# pylint: disable=multiple-statements,abstract-method
from __future__ import annotations
import abc
import collections.abc as cABC
import logging as logmod
from dataclasses import dataclass, field
from os.path import abspath, exists, expanduser, split
from typing import (Any, Callable, ClassVar, Collection, Generic, Iterable,
                    Type, Iterator, Mapping, Match, MutableMapping, Protocol,
                    Sequence, Tuple, TypeAlias, TypeVar, cast)
from types import ModuleType

logging = logmod.getLogger(__name__)

from acab import types as AT
from acab.core.decorators.engine import EnsureEngineInitialised
from acab.error.semantic import AcabSemanticException
from acab.interfaces.context import ContextSet_i
from acab.interfaces.protocols.value import AcabReducible_p

# TODO add 'Tick' functionality
ModuleComponents : TypeAlias = AT.ModuleComponents
Sen_A            : TypeAlias = AT.Sentence
Instruction      : TypeAlias = AT.Instruction
DSL_Fragment     : TypeAlias = AT.DSL_Fragment
SemanticSystem   : TypeAlias = AT.SemanticSystem
PrintSystem      : TypeAlias = AT.PrintSystem
DSL_Builder      : TypeAlias = AT.DSL_Builder
DSL_Builder_t    : TypeAlias = Type[AT.DSL_Builder]
ModuleLoader     : TypeAlias = AT.ModuleLoader

# Protocol ####################################################################
class _AcabEngine_p(Protocol):
    @abc.abstractmethod
    def load_file(self, filename:str) -> bool: pass
    @abc.abstractmethod
    def save_file(self, filename:str, *, printer:None|PrintSystem=None) -> None: pass
    @abc.abstractmethod
    def to_sentences(self) -> list[Sen_A]: pass
    @abc.abstractmethod
    def pprint(self, *, target:None|list[Sen_A]=None) -> str: pass
    @abc.abstractmethod
    def load_modules(self, *modules:ModuleType|str) -> list[ModuleComponents]: pass
    @abc.abstractmethod
    def __call__(self, *args:str|Instruction, **kwargs:Any) -> None: pass


# Interface
@dataclass #type:ignore[misc]
class AcabEngine_i(_AcabEngine_p):
    # Root components to extend
    parser         : DSL_Fragment   = field()
    semantics      : SemanticSystem = field()
    printer        : PrintSystem    = field()
    dsl_builder    : DSL_Builder_t  = field()

    # Modules to load
    modules        : list[str]      = field(default_factory=list)
    # Files to load
    load_paths     : list[str]      = field(default_factory=list)
    init_strs      : list[str]      = field(default_factory=list)

    initialised    : bool           = field(init=False, default=False)
    # Abstract fields, need to be instantiated
    _dsl           : DSL_Builder    = field(init=False)
    _module_loader : ModuleLoader   = field(init=False)

    def __post_init__(self) -> None: pass

