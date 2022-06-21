#!/usr/bin/env python3
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Protocol,
                    Sequence, Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from acab import types as AT
import acab.core.defaults.value_keys as DS
import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import AcabConfig
from acab.core.value.instruction import ProductionOperator
from acab.core.util.sentences import ProductionComponent
from acab.interfaces.bind import Bind_i
from acab.interfaces.sieve import AcabSieve
from acab.modules.context.constraint_sieve import default_sieve

config        = AcabConfig()
CONSTRAINT    = DS.CONSTRAINT
TYPE_INSTANCE = DS.TYPE_INSTANCE
ATOM          = config.prepare("Data", "TYPE_BASE")()

Bind = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()

CtxIns      = 'ContextInstance'
Constraints = 'ConstraintCollection'
Operator    = 'ProductionOperator'
Value       = AT.Value
Statement   = AT.Instruction
Sen         = AT.Sentence
Node        = AT.StructView

class ConstraintMeta(type(Protocol)):
    """
    Prepares data for the ConstraintCollection, running a sieve on the passed in
    word to group tests togther
    """
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
    """ Simple container of all constraints a word possesses,
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
        if self.operators is not None and self.operators not in stack:
            stack.append(self.operators)

        result = Bind.bind(val, stack, None)

        return result

    def __run_alphas(self, node):
        """ Run alpha tests on a node """
        # Get the (operator, params, data) trio:
        # reminder: params are x[1],
        # and because node.value is the first arg, we used words[1:]
        test_trios = [(self._get(x[0]),
                       x[1][1:] if x[1][0] == "_:node" else x[1][:],
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
            op = self._get(test[0],  stack=ctx_stack)
            params = [self._get(x, stack=ctx_stack) for x in test[1]]
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
            op = self._get(test[0])
            results.append(op(val, *test[1], ctx=ctxInst))

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
            if b_val.name != ctxInst[bind.name].name:
                raise ASErr.AcabSemanticTestFailure("Binds Failed", context=(node, self))


    def __bool__(self):
        return bool(self._test_mappings)

    def __getitem__(self, key) -> []:
        if key not in self._test_mappings:
            return []

        return self._test_mappings[key]
