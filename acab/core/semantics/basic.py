#!/usr/bin/env python3
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
# pylint: disable=multiple-statements,protected-access,too-many-ancestors
from __future__ import annotations
import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, Type, TypeAlias, TypeGuard,
                    TypeVar, cast, final, overload, runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

logging = root_logger.getLogger(__name__)

import acab.interfaces.handler_system as HS
from acab import types as AT
from acab.core.data.default_structure import QUERY
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces.sub_protocols.value import AcabReducible_p
from acab.interfaces.value import Sentence_i, Value_i
from acab.core.util import handler_system as HSImpl
from acab.interfaces import semantic as SI

Value              : TypeAlias = AT.Value
Sen_A              : TypeAlias = AT.Sentence
Instruction        : TypeAlias = AT.Instruction
Struct_A           : TypeAlias = AT.DataStructure
Node               : TypeAlias = AT.Node
Engine             : TypeAlias = AT.Engine
CtxSet             : TypeAlias = AT.CtxSet
CtxIns             : TypeAlias = AT.CtxIns
Handler_A          : TypeAlias = AT.Handler
ProductionOperator : TypeAlias = AT.Operator
ModuleComponents   : TypeAlias = AT.ModuleComponents
StructureSemantics : TypeAlias = AT.StructureSemantics
ValueSemantics     : TypeAlias = AT.ValueSemantics
StatementSemantics : TypeAlias = AT.StatementSemantics
SemanticSystem     : TypeAlias = AT.SemanticSystem


# Protocols  ##################################################################
class SemanticSystem(HSImpl.HandlerSystem, SI.SemanticSystem_i):
    ctx_set         : CtxSet
    _operator_cache : None|CtxIns

    def build_ctxset(self, ops:None|list[ModuleComponents]=None) -> CtxSet:
        """ Build a context set. Use passed in operators if provided.
        Caches operators
        """
        if bool(ops) or self._operator_cache is None:
            ctxset = self.ctx_set(ops)
        else:
            ctxset = self.ctx_set(self._operator_cache)

        if ctxset._operators is not None:
            self._operator_cache = ctxset._operators.copy()

        # Auto remove the empty context:
        ctxset.delay(ctxset.delayed_e.DEACTIVATE, ctxIns=ctxset[0].uuid)

        return ctxset

    @property
    def has_op_cache(self) -> bool:
        return self._operator_cache is not None

    def extend(self, mods:list[ModuleComponents]) -> None:
        logging.info("Extending Semantics")
        semantics = [y for x in mods for y in x.semantics]
        assert(all([isinstance(x, Semantic_Fragment) for x in semantics]))
        for sem_fragment in semantics:
            assert(sem_fragment.target_i is None or issubclass(sem_fragment.target_i, SemanticSystem_i))
            for val in sem_fragment:
                self.register(val)


class StructureSemantics(HSImpl.HandlerSystem, HSImpl.HandlerComponent, SI.StructureSemantics_i):
    """
    Dependent Semantics rely on the context they are called in to function
    and are built with specific mappings to value semantics
    """

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__}>"

    def __call__(self, sen:Sen_A, struct:Struct_A, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> None|CtxSet:
        assert(isinstance(sen, Sentence_i))
        if QUERY in sen[-1].data and bool(sen[-1].data[QUERY]):
            return self.query(sen, struct, ctxs=ctxs, data=data)

        return self.insert(sen, struct, ctxs=ctxs, data=data)



class ValueSemantics(HSImpl.HandlerComponent, SI.ValueSemantics_i):
    """
    Independent Semantics which operate on values and nodes, without
    requiring access to larger context, or engine access
    """

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__}>"

    def down(self, node:Node, *, data:None|dict[str,Any]=None) -> Value:
        return node.value


class StatementSemantics(HSImpl.HandlerComponent, SI.StatementSemantics_i):
    """
    Semantics of Higher level abstractions
    eg: Rules, Layers, Pipelines...

    AbsSems use the total semantic system to call other AbSems, or
    DepSems
    """

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__}>"
