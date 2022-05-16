#!/usr/bin/env python3

import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from enum import Enum
from uuid import UUID, uuid1

from acab import types as AT
import acab.core.defaults.value_keys as DS
import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab.core.config.config import AcabConfig
from acab.core.value.instruction import ProductionComponent, ProductionContainer
from acab.error.semantic import AcabSemanticException
from acab.interfaces.value import Sentence_i
from acab.modules.context.constraints import ConstraintCollection

config = AcabConfig()

CONSTRAINT_S     = DS.CONSTRAINT
NEGATION_S       = DS.NEGATION

CtxIns           = CtxInt.ContextInstance_i
CtxSet           = CtxInt.ContextSet_i
Constraints      = 'ConstraintCollection'
ProdComp         = ProductionComponent
ProdCon          = ProductionContainer
Operator         = 'ProductionOperator'
Value            = AT.Value
Statement        = AT.Instruction
Sen              = AT.Sentence
Node             = AT.Node
ModuleFragment   = AT.ModuleFragment
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")


@dataclass
class ContextSenBindQueryManager(CtxInt.CtxManager_i):
    """ Shared State of the current query, between different ctx insts """

    negated       : bool                                   = field(init=False, default=False)
    collect_vars  : set[str]                               = field(init=False, default_factory=set)
    ctx_clauses   : dict[UUID, list[ConstraintCollection]] = field(init=False, default_factory=dict)

    _duplication_filter : dict[Sen, list[ConstraintCollection]] = field(init=False, default_factory=dict)
    _current_constraint : ConstraintCollection                  = field(init=False, default=None)
    _current_inst       : CtxIns                                = field(init=False, default=None)
    _initial_ctxs       : list[UUID]                            = field(init=False, default_factory=list)

    def __post_init__(self):
        sen = self.target_clause
        assert(sen[0].is_var)
        self.negated = NEGATION_S in sen.data and sen.data[NEGATION_S]
        self._initial_ctxs = [x.uuid for x in self.ctxs.active_list()]
        for ctx in self.ctxs.active_list():
            sen_query = ctx[sen[0]]
            if sen_query not in self._duplication_filter:
                self._duplication_filter[sen_query] = [ConstraintCollection(x, operators=self.ctxs._operators) for x in sen_query]

            self.ctx_clauses[ctx.uuid] = self._duplication_filter[sen_query]


    def __enter__(self):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        active_list : list[CtxIns] = self.ctxs.active_list()
        [x.set_current_node(self.root_node) for x in active_list]
        return self


    def __exit__(self, exc_type, exc_value, traceback):
        self.activate_ctxs()
        if self.negated:
            # invert failed and passing
            actually_passed = self.ctxs._failed
            passed_lineages = {y for x in actually_passed for y in x.ctx._lineage}
            passed_initial  = [x for x in self._initial_ctxs if x in passed_lineages]

            actually_failed = self.ctxs.active_list(clear=True)
            [self.ctxs.fail(x, None, None, self.target_clause) for x in actually_failed]

            self.ctxs.push(passed_initial)

        # collect bindings as necessary
        self.collect()
        return False


    def __iter__(self):
        return iter(self.constraints)


    @property
    def current(self):
        return self.target_clause

    @property
    def active(self) -> Iterator[Tuple[Value, CtxIns, Node]]:
        for ctx in self.ctxs:
            self._current_inst       = ctx
            constraints              = self.ctx_clauses[ctx.uuid]
            self._current_constraint = constraints[0]
            yield (word, ctx, node)

    def maybe_test(self, results:list[Node]):
        if not bool(results):
            self.ctxs.fail(self._current_inst,
                           self._current_constraint.source,
                           self._current_inst._current,
                           self.target_clause)
            return []
        else:
            results = self.test(results)
            self.push(results)

    def test(self, possible: list[Node]):
        """
        run a word's tests on available nodes, with an instance.
        bind successes and return them

        constraints can be provided, or extracted from the test word

        """
        logging.debug(f"{repr(self)}: Testing/Extending on {len(possible)} : {possible}")
        constraints = self._current_constraint
        assert(len(possible) == 1 or bool(constraints))
        successes = []

        # Collect all nodes that pass tests
        for node in possible:
            try:
                constraints.test(node, self._current_inst)
                successes.append(node)
            except ASErr.AcabSemanticTestFailure as err:
                logging.debug(f"Tests failed on {node.value}:\n\t{err}")
                self.ctxs.fail(self._current_inst, constraints.source, node, self.target_clause)

        # Handle successes
        # success, so copy and extend ctx instance
        bound_ctxs = self._current_inst.bind(constraints.source,
                                             successes,
                                             sub_binds=constraints["sub_struct_binds"])
        return bound_ctxs

