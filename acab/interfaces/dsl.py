"""
A DSL interface for the system, which

"""
# pylint: disable=multiple-statements,abstract-method,too-many-ancestors,too-few-public-methods
from __future__ import annotations
import abc
import collections.abc as cABC
import logging as logmod
import traceback
from dataclasses import dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, Protocol,
                    Sequence, Tuple, TypeAlias, TypeVar, cast,
                    runtime_checkable)

if TYPE_CHECKING:
    import io

logging = logmod.getLogger(__name__)

from acab import types as AT
from acab.core.util.decorators.dsl import EnsureDSLInitialised
from acab.interfaces import handler_system as HS
from acab.interfaces.protocols import handler_system as HSubP

Parser           : TypeAlias = AT.Parser
Sentence         : TypeAlias = AT.Sentence
Query            : TypeAlias = AT.Container
ModuleComponents : TypeAlias = AT.ModuleComponents
DSL_Spec_A       : TypeAlias = AT.DSL_Spec
File             : TypeAlias = 'io.TextIOBase'

class DSL_Parser_i(Protocol):
    @abc.abstractmethod
    def parse_string(self, string:str) -> list[Sentence]: pass
    @abc.abstractmethod
    def parse_file(self, f:str) -> list[Sentence]: pass
    @abc.abstractmethod
    def __call__(self, *args:str) -> list[Sentence]: pass

# Protocols ###################################################################
@runtime_checkable
class _DSL_Spec_p(HSubP.HandlerSpec_p, Protocol):
    @abc.abstractmethod
    def extend_spec(self, spec:DSL_Spec_A) -> None: pass

@runtime_checkable
class _DSL_Builder_p(HSubP.HandlerSystem_p, DSL_Parser_i, Protocol):
    @abc.abstractmethod
    def build(self) -> None: pass
    @abc.abstractmethod
    def parse(self, s:str) -> list[Sentence]: pass


# Interfaces:
@dataclass #type:ignore[misc]
class DSL_Handler_i(HS.Handler_i):
    """ Register a function for handling a DSL setup signal.
    ie: This function is run to set a pyparsing `Forward`"""
    func : Parser = field(kw_only=True)

@dataclass #type:ignore[misc]
class DSL_Spec_i(HS.HandlerSpec_i, _DSL_Spec_p):
    """
    Register a signal into a DSL,
    """
    struct : set[Parser] = field(default_factory=set)


@dataclass #type:ignore[misc]
class DSL_Builder_i(HS.HandlerSystem_i, _DSL_Builder_p):
    _parsers_initialised  : bool           = field(init=False, default=False)
    _loaded_DSL_fragments : dict[Any, Any] = field(init=False, default_factory=dict)
