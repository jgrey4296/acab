#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Dict, Generic,
                    Iterable, Iterator, List, Mapping, Match, MutableMapping,
                    Optional, Protocol, Sequence, Set, Tuple, TypeVar, Union,
                    cast)

import acab.core.defaults.value_keys as DS
import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import AcabConfig
from acab import types as AT
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import ProductionOperator
from acab.interfaces.bind import Bind_i
from acab.interfaces.sieve import AcabSieve
from acab.modules.context.constraint_sieve import default_sieve
from acab.core.metaclasses.constraint import ConstraintMeta

if TYPE_CHECKING:
    CtxIns      = 'ContextInstance'
    Constraints = 'ConstraintCollection'
    Operator    = 'ProductionOperator'
    Value       = AT.Value
    Statement   = AT.Instruction
    Sen         = AT.Sentence
    Node        = AT.StructView

logging       = logmod.getLogger(__name__)
config        = AcabConfig()
ATOM          = config.attr.Data.TYPE_BASE
Bind          = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()

CONSTRAINT    = DS.CONSTRAINT
TYPE_INSTANCE = DS.TYPE_INSTANCE

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
        stack = stack or []
        if self.operators is not None:
            stack.append(self.operators)

        for ctx in stack:
            result = Bind.bind(val, ctx)
            if result != val:
                return result

        return result

    def __run_alphas(self, node):
        """ Run alpha tests on a node """
        # Get the (operator, params, data) trio:
        # reminder: params are x[1][1:],
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
            params = [self._get(x, stack=ctx_stack) for x in test[1][1:]]
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
            results.append(op(val, *test[1][1:], ctx=ctxInst))

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

    def __getitem__(self, key) -> list:
        if key not in self._test_mappings:
            return []

        return self._test_mappings[key]
