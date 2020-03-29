"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators
"""
from .value import PyRuleValue
from .sentence import Sentence
from py_rule.util import OPERATOR_S, STATEMENT_S

class ProductionOperator(PyRuleValue):
    """ The Base Operator Class """
    # operator calls are late resolving,
    # and allow type checking to resolve earlier

    def __init__(self, num_params=2, infix=False, type_str=OPERATOR_S):
        super().__init__(type_str=type_str)
        # TODO this can be done using subclass DFS
        self._op_str = self.__class__.__name__
        self._num_params = num_params
        # TODO use infix
        self._infix = infix

    def __call__(self):
        raise NotImplementedError()

    def __str__(self):
        raise NotImplementedError()

    def __repr__(self):
        raise NotImplementedError()


class ProductionComponent(PyRuleValue):
    """ Pairs a an operator with some bindings """

    def __init__(self, op_str, params, type_str=None):
        super().__init__(type_str=type_str)
        # The lookup string of the operator:
        self._op = op_str
        # A List of parameters for the Component
        assert(isinstance(params, list))
        self._params = params[:]

    def __str__(self):
        raise NotImplementedError()

    def __repr__(self):
        raise NotImplementedError()

    def __call__(self):
        raise NotImplementedError()

    def __refine_op_func(self, op_str):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        raise NotImplementedError()

    def copy(self):
        return self.__class__(self._op, self._params, type_str=self._type)

    def to_sentence(self):
        raise NotImplementedError()

    def verify_op(self):
        """ Complains if the operator is not a defined Operator Enum """
        raise NotImplementedError()


class ProductionContainer(PyRuleValue):

    def __init__(self, clauses, type_str=STATEMENT_S):
        super().__init__(type_str=type_str)
        self._clauses = clauses[:]

    def __str__(self):
        raise NotImplementedError()

    def __repr__(self):
        raise NotImplementedError()

    def __len__(self):
        return len(self._clauses)

    def __call__(self, ctx):
        assert(isinstance(ctx, dict))
        for x in self._clauses:
            ctx[x._rebind._value] = x(ctx)

        return ctx

    def __iter__(self):
        for x in self._clauses:
            yield x
    def to_sentences(self):
        return [x.to_sentence() for x in self._clauses]

    def verify_ops(self):
        for x in self._clauses:
            x.verify_op()

    def expand_bindings(self, bindings):
        raise NotImplementedError()

    def copy(self):
        return self.__class__([x.copy() for x in self._clauses], type_str=self._type)
