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
import acab.interfaces.context as CtxInt
import acab.error.semantic as ASErr
from acab import AcabConfig
from acab.core.value.instruction import (ProductionComponent,
                                                        ProductionContainer)
from acab.interfaces.value import Sentence_i
from acab.error.semantic import AcabSemanticException
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
Sen              = Sentence_i
Node             = AT.Node
ModuleFragment   = AT.ModuleFragment
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")


@dataclass
class ContextWalkManager(CtxInt.CtxManager_i):
    """ Shared State of a data structure walk, between different ctx insts """

    collect_vars  : set[str]                   = field(init=False, default_factory=set)
    constraints   : list[ConstraintCollection] = field(init=False, default_factory=list)

    _current_inst       : CtxIns               = field(init=False, default=None)

    def __post_init__(self):
        sen = self.target_clause
        start = 0
        if sen[0].is_at_var:
            start = 1
        constraints = [ConstraintCollection(x, operators=self.ctxs._operators) for x in sen[start:]]
        self.constraints.extend(constraints)

    def __enter__(self):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        active_list : list[CtxIns] = self.ctxs.active_list()

        if not self.target_clause[0].is_at_var:
            [x.set_current_node(self.root_node) for x in active_list]
        else:
            root_word = self.target_clause[0]
            assert(root_word.is_at_var)
            if isinstance(root_word, Sentence_i):
                root_word = root_word[0]

            [x.set_current_binding(root_word) for x in active_list]

        return self


    def __exit__(self, exc_type, exc_value, traceback):
        self.activate_ctxs()
        # collect bindings as necessary
        self.collect()
        return False


    def __iter__(self):
        return iter(self.constraints)

    @property
    def current(self) -> Iterator[CtxIns]:
        active_ctxs = self.ctxs.active_list(clear=True)
        for ctx in active_ctxs:
            self._current_inst = ctx
            self.activate_ctxs()
            yield ctx


    def maybe_test(self, possible:list[Node]):
        if not bool(possible):
            return

        results = self.test(possible)
        self.queue_ctxs(results)

    def test(self, possible: list[Node]):
        """
        run a word's tests on available nodes, with an instance.
        bind successes and return them

        constraints can be provided, or extracted from the test word

        """

        # [ctxs.test(ctxIns, accessible, x) for x in tests]

        logging.debug(f"{repr(self)}: Testing/Extending on {len(possible)} : {possible}")
        bound_ctxs = []

        for constraints in self.constraints:
            successes   = []
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
            bound_ctxs += self._current_inst.bind(constraints.source,
                                                  successes,
                                                  sub_binds=constraints["sub_struct_binds"])

        return bound_ctxs


