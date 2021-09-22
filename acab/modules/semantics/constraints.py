#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab.abstract.config import GET
import acab.abstract.interfaces.context as CtxInt
from acab.abstract.core.production_abstractions import ProductionComponent
import acab.error.acab_semantic_exception as ASErr

config = GET()
CONSTRAINT_S = config.prepare("Parse.Structure", "CONSTRAINT")()

CtxIns      = 'ContextInstance'
Constraints = 'ConstraintCollection'
ProdComp    = 'ProductionComponent'
Operator    = 'ProductionOperator'
Value       = 'AcabValue'
Statement   = 'AcabStatement'
Sentence    = 'Sentence'
Node        = 'AcabNode'

@dataclass(frozen=True)
class ConstraintCollection(CtxInt.Constraint_i):
    """ Simple container of all ProductionComponent constraints a word possesses,
    separated into subtypes """

    _alphas      : List[ProdComp]      = field(default_factory=list)
    _betas       : List[ProdComp]      = field(default_factory=list)
    _bind        : Value               = field(default=None)
    _annotations : List[ProdComp]      = field(default_factory=list)
    _callables   : List[ProdComp]      = field(default_factory=list)
    _variables   : List[Value]         = field(default_factory=list)
    _operators   : CtxIns              = field(default=None)

    @staticmethod
    def build(word, operators) -> Constraints:
        """ Split tests into (alphas, betas, sub_binds),
        Also connect Components to actual _operators
        """
        bind                 = None
        if word.is_var:
            bind = word

        if CONSTRAINT_S not in word.data:
            return ConstraintCollection(_bind=bind)

        constraints          = word.data[CONSTRAINT_S]
        annotations          = set()
        callable_annotations = []
        alphas               = []
        betas                = []
        variable_ops         = []

        for c in constraints:
            is_prod_comp = isinstance(c, ProductionComponent)
            if c.is_var:
                variable_ops.append(c)
            elif is_prod_comp and any([p.is_var for p in c.params]):
                betas.append(c)
            elif is_prod_comp:
                alphas.append(c)
            elif isinstance(c, Callable):
                callable_annotations.append(c)
            else:
                annotations.add(c)


        return ConstraintCollection(alphas,
                                    betas,
                                    bind,
                                    annotations,
                                    callable_annotations,
                                    variable_ops,
                                    operators)


    def test_all(self, node, ctx):
        # run alpha tests
        self.alphas(node)
        # run beta tests
        self.betas(node, ctx)
        # run bind test
        self.binds(node, ctx)
        # run callable tests
        self.callables(node, ctx)

    def alphas(self, node):
        """ Run alpha tests on a node """
        # Get the (operator, params, data) trio:
        test_trios = [(self._operators[x.op],
                       x.params,
                       x.data) for x in self._alphas]
        # Perform the tests:
        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Alphas Failed", (node, self))

    def betas(self, node, ctxInst):
        """ Run Beta Tests on a node and context isntance """
        test_trios = []
        for test in self._betas:
            op     = self._operators[test.op]
            params = [ctxInst[x] for x in test.params]
            trio   = (op, params, test.data)
            test_trios.append(trio)

        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Betas Failed", (node, self))

    def binds(self, node, ctxInst):
        """ Check node against prior binding """
        if self._bind is None:
            return
        if self._bind not in ctxInst:
            return

        if node.value != ctxInst[self._bind]:
            raise ASErr.AcabSemanticTestFailure("Binds Failed", (node, self))

    def callables(self, node, ctxInst):
        test_trios = []
        for test in self._callables:
            op     = self._operators[test.op]
            params = [ctxInst[x] for x in test.params]
            trio   = (op, params, test.data)
            test_trios.append(trio)

        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Callables failed", (node, self))
