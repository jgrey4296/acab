#!/usr/bin/env python3
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Protocol,
                    Sequence, Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import AcabConfig
from acab.core.value.instruction import ProductionComponent, ProductionOperator
from acab.core.value.sentence import Sentence
from acab.interfaces.sieve import AcabSieve
from acab.modules.context.constraint_sieve import default_sieve
from acab.interfaces.bind import Bind_i

config        = AcabConfig()
CONSTRAINT    = config.prepare("Value.Structure", "CONSTRAINT")
TYPE_INSTANCE = config.prepare("Value.Structure", "TYPE_INSTANCE")()
ATOM          = config.prepare("Data", "TYPE_BASE")()

Bind = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()

CtxIns      = 'ContextInstance'
Constraints = 'ConstraintCollection'
ProdComp    = 'ProductionComponent'
Operator    = 'ProductionOperator'
Value       = 'AcabValue'
Statement   = 'Instruction'
Sen         = 'Sentence'
Node        = 'AcabNode'

class ConstraintMeta(type(Protocol)):
    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(ConstraintMeta, cls).__init__(name, bases, data)

    def __call__(cls, word, *, operators=None, sieve_fns:list[Callable]=None):
        if operators is not None:
            cls.operators = operators

        if sieve_fns is None:
            sieve = cls.sieve
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


        return super(ConstraintMeta, cls).__call__(word, tests)


@dataclass(frozen=True)
class ConstraintCollection(CtxInt.Constraint_i, metaclass=ConstraintMeta):
    """ Simple container of all ProductionComponent constraints a word possesses,
    separated into subtypes """
    sieve           : ClassVar[list[Callable]] = AcabSieve(default_sieve)
    operators       : ClassVar[CtxIns]         = None


    def test(self, node, ctx):
        # TODO for key in self._test_order:...
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

    def _get(self, val, *, stack=None):
        """
        Retrieve a value from a stack of context instances.
        stack auto-includes CC's operators ctx inst.
        """
        if self.operators is None and not bool(stack):
            return val

        # TODO separate this into sieve, then move to contextset?
        if stack is None:
            stack = []
        if self.operators not in stack:
            stack.append(self.operators)

        result = Bind.bind(val, stack, None)

        return result

    def __run_alphas(self, node):
        """ Run alpha tests on a node """
        # Get the (operator, params, data) trio:
        test_trios = [(self._get(x.op),
                       x.params,
                       x.data) for x in self._test_mappings["alpha"]]
        # Perform the tests:
        results = [op(node.value, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Alphas Failed", context=(node, self))

    def __run_betas(self, node, ctxInst):
        """ Run Beta Tests on a node and context isntance """
        test_trios = []
        ctx_stack = [ctxInst]
        for test in self._test_mappings["beta"]:
            op = self._get(test.op, stack=ctx_stack)
            params = [self._get(x, stack=ctx_stack) for x in test.params]
            trio   = (op, params, test.data)
            test_trios.append(trio)

        val = self._get(node.value, stack=ctx_stack)
        results = [op(val, *pars, data=data) for op,pars,data in test_trios]
        if not all(results):
            raise ASErr.AcabSemanticTestFailure("Betas Failed", context=(node, self, ctxInst))

    def __run_substruct_tests(self, node, ctxInst):
        results = []
        val = self._get(node.value, stack=[ctxInst])
        for test in self._test_mappings["sub_struct_tests"]:
            op = self._get(test.op)
            results.append(op(val, *test.params, ctx=ctxInst))

        if not all(results):
            # binds can't succeed if tests fail
            raise ASErr.AcabSemanticTestFailure("Sub Structural Tests Failed", context=(node, self, ctxInst))

    def __run_name(self, node, ctxInst):
        """ Check node against prior binding """
        b_val = ctxInst[node.value]
        for bind in self._test_mappings["name"]:
            # TODO use bind
            if b_val.is_var or ctxInst[bind].is_var:
                continue
            if b_val != ctxInst[bind]:
                raise ASErr.AcabSemanticTestFailure("Binds Failed", context=(node, self))


    def __bool__(self):
        return bool(self._test_mappings)

    def __getitem__(self, key) -> []:
        if key not in self._test_mappings:
            return []

        return self._test_mappings[key]
