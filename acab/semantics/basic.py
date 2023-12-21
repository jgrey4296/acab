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
##-- imports
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, Type, TypeAlias, TypeGuard,
                    TypeVar, cast, final, overload, runtime_checkable)

import acab.core.defaults.value_keys as DS
import acab.interfaces.handler_system as HS
import acab
from acab.core.defaults.value_keys import QUERY
from acab.core.util.part_implementations import handler_system as HSImpl
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException
from acab.interfaces import semantic as SI
from acab.interfaces.bind import Bind_i
from acab.interfaces.fragments import Semantic_Fragment_i
from acab.interfaces.value import Sentence_i, Value_i

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    from acab import types as AT
    CtxIns             : TypeAlias = AT.CtxIns
    CtxSet             : TypeAlias = AT.CtxSet
    Engine             : TypeAlias = AT.Engine
    Handler_A          : TypeAlias = AT.Handler
    Instruction        : TypeAlias = AT.Instruction
    ModuleFragment     : TypeAlias = AT.ModuleFragment
    Node               : TypeAlias = AT.Node
    ProductionOperator : TypeAlias = AT.Operator
    SemanticSystem     : TypeAlias = AT.SemanticSystem
    Sen_A              : TypeAlias = AT.Sentence
    StatementSemantics : TypeAlias = AT.StatementSemantics
    Struct_A           : TypeAlias = AT.DataStructure
    StructureSemantics : TypeAlias = AT.StructureSemantics
    Value              : TypeAlias = AT.Value
    ValueSemantics     : TypeAlias = AT.ValueSemantics

##-- end imports

config = acab.config
# TODO import
Bind   = config.imports.specific.bind

# Protocol Implementations #############################################################
class SemanticSystem(HSImpl.HandlerSystem, SI.SemanticSystem_i):
    ctx_set         : CtxSet
    _operator_cache : None|CtxIns


    def __repr__(self):
        ops = ""
        if bool(self._operator_cache):
            ops = f", operators={len(self._operator_cache)}"

        return f"<{self.__class__.__name__} handlers={len(self.handler_specs)}, sieve={len(self.sieve)}{ops}>"

    def build_ctxset(self, ops:None|list[ModuleFragment]=None) -> CtxSet:
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
        ctxset.delay(ctxset.delayed_e.DEACTIVATE, val=ctxset[0].uuid)

        return ctxset

    @property
    def has_op_cache(self) -> bool:
        return self._operator_cache is not None

    def extend(self, mods:list[ModuleFragment]) -> None:
        logging.debug("Extending Semantics")
        semantics = [y for x in mods for y in x.semantics]
        assert(all([isinstance(x, Semantic_Fragment_i) for x in semantics]))
        for sem_fragment in semantics:
            assert(sem_fragment.target_i is None or issubclass(sem_fragment.target_i, SI.SemanticSystem_i))
            for val in sem_fragment:
                self.register(val)


class StructureSemantics(HSImpl.HandlerSystem, HSImpl.HandlerComponent, SI.StructureSemantics_i):
    """
    Dependent Semantics rely on the context they are called in to function
    and are built with specific mappings to value semantics
    """
    def __call__(self, sen:Sen_A, struct:Struct_A, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> None|CtxSet:
        assert(isinstance(sen, Sentence_i))
        # TODO move query annotation to sentence, not the last word
        if QUERY in sen.data and bool(sen.data[QUERY]):
            logging.debug("Firing Query Semantics")
            return self.query(sen, struct, ctxs=ctxs, data=data)

        if DS.NEGATION in sen.data and sen.data[DS.NEGATION]:
            logging.debug("Firing Delete Semantics")
            self.delete(sen, struct, data=data, ctxs=ctxs)
            return ctxs

        logging.debug("Firing Insert Semantics")
        for ctx in ctxs:
            bound_sen = Bind.bind(sen, ctx)
            self.insert(bound_sen, struct, ctxs=ctxs, data=data)


class ValueSemantics(HSImpl.HandlerComponent, SI.ValueSemantics_i):
    """
    Independent Semantics which operate on values and nodes, without
    requiring access to larger context, or engine access
    """

    def down(self, node:Node, *, data:None|dict[str,Any]=None) -> Value:
        return node.value


class StatementSemantics(HSImpl.HandlerComponent, SI.StatementSemantics_i):
    """
    Semantics of Higher level abstractions
    eg: Rules, Layers, Pipelines...

    AbsSems use the total semantic system to call other AbSems, or
    DepSems
    """
    pass
