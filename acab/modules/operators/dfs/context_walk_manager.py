#!/usr/bin/env python3

#!/usr/bin/env python3

import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from enum import Enum
from uuid import UUID, uuid1

import acab.interfaces.context as CtxInt
import acab.error.semantic_exception as ASErr
from acab.core.config import GET
from acab.core.data.production_abstractions import (ProductionComponent,
                                                        ProductionContainer)
from acab.interfaces.value import Sentence_i
from acab.error.semantic_exception import AcabSemanticException
from acab.modules.context.constraints import ConstraintCollection

config = GET()

CONSTRAINT_S     = config.prepare("Value.Structure", "CONSTRAINT")()
NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()

CtxIns           = CtxInt.ContextInstance_i
CtxSet           = CtxInt.ContextSet_i
Constraints      = 'ConstraintCollection'
ProdComp         = ProductionComponent
ProdCon          = ProductionContainer
Operator         = 'ProductionOperator'
Value            = 'AcabValue'
Statement        = 'AcabStatement'
Sen              = Sentence_i
Node             = 'AcabNode'
ModuleComponents = "ModuleComponents"
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")


@dataclass
class ContextWalkManager:
    """ Shared State of a data structure walk, between different ctx insts """

    walk_spec     : Sen                = field()
    root_node     : Node               = field()
    ctxs          : CtxSet             = field()

    collect_vars  : Set[str]                   = field(init=False, default_factory=set)
    constraints   : List[ConstraintCollection] = field(init=False, default_factory=list)

    _current_inst       : CtxIns               = field(init=False, default=None)

    def __post_init__(self):
        sen = self.walk_spec
        start = 0
        if sen[0].is_at_var:
            start = 1
        constraints = [ConstraintCollection.build(x, operators=self.ctxs._operators) for x in sen[start:]]
        self.constraints.extend(constraints)

    def __enter__(self):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        active_list : List[CtxIns] = self.ctxs.active_list()

        if not self.walk_spec[0].is_at_var:
            [x.set_current_node(self.root_node) for x in active_list]
        else:
            assert(self.walk_spec[0].is_at_var)
            root_word = self.walk_spec[0]
            [x.set_current_binding(root_word) for x in active_list]

        return self


    def __exit__(self, exc_type, exc_value, traceback):
        # collect bindings as necessary
        self.collect()
        # TODO handle exception


    def __iter__(self):
        return iter(self.constraints)



    @property
    def active(self) -> Iterator[Node]:
        active_ctxs = self.ctxs.active_list(clear=True)
        for ctx in active_ctxs:
            self._current_inst = ctx
            yield [ctx._current]


    def test_and_update(self, results:List[Node]):
        self.test(results)

    def test(self, possible: List[Node]):
        """
        run a word's tests on available nodes, with an instance.
        bind successes and return them

        constraints can be provided, or extracted from the test word

        """

        # [ctxs.test(ctxIns, accessible, x) for x in tests]

        logging.debug(f"{repr(self)}: Testing/Extending on {len(possible)} : {possible}")

        for constraints in self.constraints:
            successes   = []
            # Collect all nodes that pass tests
            for node in possible:
                try:
                    constraints.test(node, self._current_inst)
                    successes.append(node)
                except ASErr.AcabSemanticTestFailure as err:
                    logging.debug(f"Tests failed on {node.value}:\n\t{err}")
                    self.ctxs.fail(self._current_inst, constraints.source, node, self.walk_spec)

            # Handle successes
            # success, so copy and extend ctx instance
            bound_ctxs = self._current_inst.bind(constraints.source,
                                                 successes,
                                                 sub_binds=constraints["sub_struct_binds"])
            self.ctxs.push(bound_ctxs)

        return bound_ctxs

    def collect(self):
        """
        Context collecton specific vars.
        Flattens many contexts into one, with specified variables
        now as lists accumulated from across the contexts.

        Semantics of collect:
        0[ctxs]0 -> fail
        1[ctxs]n -> 1[α]1
        where
        α : ctx = ctxs[0] ∪ { β : ctx[β] for ctx in ctxs[1:] }
        β : var to collect


        """
        if not bool(self.collect_vars):
            return

        # select instances with bindings
        # Merge into single new instance
        # replace
        raise NotImplementedError()
