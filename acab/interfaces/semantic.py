"""
Semantics:
Subdivides into *Complete Systems*, *Incomplete Mixins* and *Handlers*

All semantic systems should be able to lift basic sentences up to their preferred internal data format.
And reduce those internal formats back down to basic sentences.

SemanticMap also provide the ability to map a value or node to particular semantics,
and specifies *how* to search for the correct mapping.

Meanwhile ValueSemantics_i are concerned only with the values and structures they have control over.

*Dependent* Semantics factor in contexts and a reference to the engine.


"""
# pylint: disable=multiple-statements,too-many-ancestors,invalid-sequence-index,abstract-method,arguments-differ
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, Type, TypeAlias, TypeGuard,
                    TypeVar, cast, final, overload,
                    runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

logging = logmod.getLogger(__name__)

import acab.interfaces.handler_system as HS
from acab import types as AT
from acab.core.value.default_structure import QUERY
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.protocols import handler_system as HSubP
from acab.interfaces.protocols.value import AcabReducible_p
from acab.interfaces.value import Sentence_i, Value_i

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


# Data  #######################################################################
@dataclass #type:ignore[misc]
class Semantic_Fragment_i(HS.HandlerFragment_i):
    """ Dataclass of Semantic Handlers to be added to the system, and any
    data they require
    """
    target_i : None | Type[SemanticSystem] = field(default=None) #type:ignore[assignment]

# Protocols  ##################################################################
class _SemanticSystem_p(HSubP.HandlerSystem_p, AcabReducible_p, Protocol):
    @abc.abstractmethod
    def build_ctxset(self, ops:None|list[ModuleComponents]=None) -> CtxSet: pass

    @property
    @abc.abstractmethod
    def has_op_cache(self) -> bool: pass

    @abc.abstractmethod
    def __call__(self, *instructions:Instruction, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None, **kwargs:Any) -> CtxSet: pass



class _StructureSemantics_p(HSubP.HandlerComponent_p, HSubP.HandlerSystem_p, Protocol):
    """
    Dependent Semantics rely on the context they are called in to function
    and are built with specific mappings to value semantics
    """
    @overload
    @abc.abstractmethod
    def __call__(self, sen:Sen_A, struct:Struct_A, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> None|CtxSet: pass

    @overload
    @abc.abstractmethod
    def __call__(self, *args:Any, **kwargs:Any) -> Any: pass

    @abc.abstractmethod
    def insert(self, sen:Sen_A, struct:Struct_A, *, data:None|dict[str,Any], ctxs:CtxSet) -> None: pass

    @abc.abstractmethod
    def query(self, sen:Sen_A, struct:Struct_A, *, data:None|dict[str,Any], ctxs:CtxSet) -> CtxSet: pass

    @abc.abstractmethod
    def compatible(self, struct: Struct_A) -> bool: pass


    @abc.abstractmethod
    def to_sentences(self, struct: Struct_A|Node, *, data=None, ctxs=None): pass
class _ValueSemantics_p(HSubP.HandlerComponent_p, Protocol):
    """
    Independent Semantics which operate on values and nodes, without
    requiring access to larger context, or engine access
    """
    @abc.abstractmethod
    def down(self, node:Node, *, data:None|dict[str,Any]=None) -> Value: pass

    @abc.abstractmethod
    def update(self, node:Node, term:Value, *, data:None|dict[Any, Any]=None) -> None: pass

    @abc.abstractmethod
    def make(self, val:Value, *, data:None|dict[Any,Any]=None) -> Node: pass

    @abc.abstractmethod
    def up(self, node:Node, *, data:None|dict[str,Any]=None) -> Node: pass

    @abc.abstractmethod
    def access(self, node:Node, term:Value, *, data:None|dict[Any,Any]=None) -> list[Node]: pass

    @abc.abstractmethod
    def insert(self, node:Node, new_node:Node, *, data:None|dict[Any,Any]=None) -> Node: pass

    @abc.abstractmethod
    def remove(self, node:Node, term:Value, *, data:None|dict[Any,Any]=None) -> Node: pass



class _StatementSemantics_p(HSubP.HandlerComponent_p, Protocol):
    """
    Semantics of Higher level abstractions
    eg: Rules, Layers, Pipelines...

    AbsSems use the total semantic system to call other AbSems, or
    DepSems
    """
    @abc.abstractmethod
    def verify(self, instruction:Instruction) -> bool: pass
    @overload
    @abc.abstractmethod
    def __call__(self, instruction:Instruction, semSys:SemanticSystem, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet: pass
    @overload
    @abc.abstractmethod
    def __call__(self, *args:Any, **kwargs:Any) -> Any: pass

# Interfaces ##################################################################
@dataclass #type:ignore[misc]
class SemanticSystem_i(HS.HandlerSystem_i, _SemanticSystem_p):
    """
    Map Instructions to Instruction/Struct_A Semantics
    """
    # TODO possibly re-add hooks / failure handling
    # TODO add a system specific logging handler
    ctx_set         : CtxSet           = field(kw_only=True)
    _operator_cache : None|CtxIns    = field(init=False, default=None)

@dataclass
class StructureSemantics_i(HS.HandlerComponent_i, HS.HandlerSystem_i, _StructureSemantics_p): pass
class ValueSemantics_i(HS.HandlerComponent_i, _ValueSemantics_p): pass
class StatementSemantics_i(HS.HandlerComponent_i, _StatementSemantics_p): pass
