#!/usr/bin/env python3
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

import acab.abstract.interfaces.context as CtxInt
import acab.error.acab_semantic_exception as ASErr
from acab.abstract.config import GET
from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionOperator)
from acab.abstract.core.values import Sentence
from acab.modules.context.constraint_sieve import default_sieve
from acab.abstract.interfaces.sieve import AcabSieve

config = GET()
CONSTRAINT    = config.prepare("Value.Structure", "CONSTRAINT")
TYPE_INSTANCE = config.prepare("Value.Structure", "TYPE_INSTANCE")()
ATOM          = config.prepare("Data", "TYPE_BOTTOM_NAME")()

CtxIns      = 'ContextInstance'
Constraints = 'ConstraintCollection'
ProdComp    = 'ProductionComponent'
Operator    = 'ProductionOperator'
Value       = 'AcabValue'
Statement   = 'AcabStatement'
Sen         = 'Sentence'
Node        = 'AcabNode'


@dataclass(frozen=True)
class ConstraintCollection(CtxInt.Constraint_i):
    """ Simple container of all ProductionComponent constraints a word possesses,
    separated into subtypes """

    source         : Value                     = field()
    _test_mappings : Dict[str, List[Callable]] = field()

    sieve           : ClassVar[List[Callable]] = AcabSieve(default_sieve)
    operators       : ClassVar[CtxIns]         = None

    @staticmethod
    def build(word, operators=None, sieve_fns:List[Callable]=None):
        """ Run the sieve on a word to generate the test set groupings """
        if operators is not None:
            ConstraintCollection.operators = operators

        if sieve_fns is None:
            sieve = ConstraintCollection.sieve
        else:
            sieve = AcabSieve(seive_fns)

        tests = {}
        for result in sieve.fifo_collect(word):
            stop, x, y = result
            if x not in tests:
                tests[x] = []

            tests[x] += y
            if stop:
                break


        return ConstraintCollection(word, tests)


    def test(self, node, ctx):
        # run alpha tests
        if "alpha" in self._test_mappings:
            self.__run_alphas(node)
        # run beta tests
        if "beta" in self._test_mappings:
            self.__run_betas(node, ctx)
        # Run substruct tests
        if "sub_struct_tests" in self._test_mappings:
            self.__run_substruct_tests(node, ctx)
        # run bind test
        if "name" in self._test_mappings:
            self.__run_name(node, ctx)

    def _get(self, val, stack=None):
        """
        Retrieve a value from a stack of a context instance.
        stack auto-includes CC's operators ctx inst.
        """
        if stack is None:
            stack = []
        # TODO separate this into sieve, then move to contextset?
        stack.append(self.operators)

        for ctx in stack:
            if val in ctx:
                return ctx[val]

        if isinstance(val, Sentence) and val.has_var:
            return val.bind(stack[0])

        return val

    def __run_alphas(self, node):
        """ Run alpha tests on a node """
        # Get the (operator, params, data) trio:
        test_trios = [(self._get(x.op),
                       x.params,
                       x.data) for x in self._test_mappings["alpha"]]
        # Perform the tests:
        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Alphas Failed", (node, self))

    def __run_betas(self, node, ctxInst):
        """ Run Beta Tests on a node and context isntance """
        test_trios = []
        ctx_stack = [ctxInst]
        for test in self._test_mappings["beta"]:
            op = self._get(test.op, ctx_stack)
            params = [self._get(x, ctx_stack) for x in test.params]
            trio   = (op, params, test.data)
            test_trios.append(trio)

        val = self._get(node.value, ctx_stack)
        results = [op(val, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Betas Failed", (node, self, ctxInst))

    def __run_substruct_tests(self, node, ctxInst):
        results = []
        val = self._get(node.value, [ctxInst])
        for test in self._test_mappings["sub_struct_tests"]:
            op = self._get(test.op)
            results.append(op(val, ctxInst, *test.params))

        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Sub Structural Binds Failed", (node, self, ctxInst))

    def __run_name(self, node, ctxInst):
        """ Check node against prior binding """
        b_val = ctxInst[node.value]
        for bind in self._test_mappings["name"]:
            if b_val.is_var or ctxInst[bind].is_var:
                continue
            if b_val != ctxInst[bind]:
                raise ASErr.AcabSemanticTestFailure("Binds Failed", (node, self))


    def __bool__(self):
        return bool(self._test_mappings)

    def __getitem__(self, key) -> []:
        if key not in self._test_mappings:
            return []

        return self._test_mappings[key]
