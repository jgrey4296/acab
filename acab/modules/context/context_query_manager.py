#!/usr/bin/env python3

import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from enum import Enum
from uuid import UUID, uuid1

import acab.core.value.default_structure as DS
import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import AcabConfig
from acab.core.value.instruction import (ProductionComponent,
                                         ProductionContainer)
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
Value            = 'AcabValue'
Statement        = 'Instruction'
Sen              = Sentence_i
Node             = 'AcabNode'
ModuleComponents = "ModuleComponents"
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")


@dataclass
class ContextQueryManager(CtxInt.CtxManager_i):
    """ Shared State of the current query, between different ctx insts """

    negated       : bool                       = field(init=False, default=False)
    collect_vars  : set[str]                   = field(init=False, default_factory=set)
    constraints   : list[ConstraintCollection] = field(init=False, default_factory=list)

    _current_constraint : ConstraintCollection = field(init=False, default=None)
    _current_inst       : CtxIns               = field(init=False, default=None)
    _initial_ctxs       : list[UUID]           = field(init=False, default_factory=list)

    def __post_init__(self):
        sen = self.target_clause
        self.negated = NEGATION_S in sen.data and sen.data[NEGATION_S]
        constraints = [ConstraintCollection(x, operators=self.ctxs._operators) for x in sen]
        self.constraints.extend(constraints)
        self._initial_ctxs = [x.uuid for x in self.ctxs.active_list()]

    def __repr__(self):
        return f"<{self.__class__.__name__} {'-' if self.negated else '+'}Clause:{self.target_clause} Root={self.root_node} Size={len(self.ctxs)}>"

    def __enter__(self):
        """
        set all instances to start at node,
        unless start_word is an at_binding,
        in which case get the bound node
        handle negated behaviour
        """
        active_list : list[CtxIns] = self.ctxs.active_list()

        if self.target_clause is None or not self.target_clause[0].is_at_var:
            [x.set_current_node(self.root_node) for x in active_list]
        else:
            assert(self.target_clause[0].is_at_var)
            root_word = self.target_clause[0]
            [x.set_current_binding(root_word) for x in active_list]

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
        # TODO handle exception
        return False


    def __iter__(self):
        return iter(self.constraints)

    @property
    def current(self) -> Iterator[Value]:
        clause_constraints = self.constraints

        for constraints in clause_constraints:
            self._current_constraint = constraints
            self.activate_ctxs()
            yield constraints.source

    @property
    def active(self) -> Iterator[Tuple[Value, CtxIns, Node]]:
        active_ctxs = self.ctxs.active_list(clear=True)
        for ctx in active_ctxs:
            self._current_inst = ctx
            bound_word = ctx[self._current_constraint.source]

            if bound_word.is_var:
                bound_word = None

            yield (bound_word, ctx, ctx._current)


    def maybe_test(self, possible:list[Node]):
        if not bool(possible):
            self.ctxs.fail(self._current_inst,
                           self._current_constraint.source,
                           self._current_inst._current,
                           self.target_clause)
        else:
            results = self.test(possible)
            self.queue_ctxs(results)

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

