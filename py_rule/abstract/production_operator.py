"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators
"""
from py_rule.util import OPERATOR_S, STATEMENT_S
from py_rule.abstract.printing import util as PrU

from .value import PyRuleValue


class ProductionOperator(PyRuleValue):
    """ The Base Operator Class """
    # operator calls are late resolving,
    # and allow type checking to resolve earlier

    def __init__(self, num_params=2, infix=False, type_str=OPERATOR_S):
        super().__init__(self.__class__.__name__,
                         type_str=type_str)
        # TODO this can be done using subclass DFS
        self._num_params = num_params
        # TODO use infix
        self._infix = infix

    def __call__(self):
        raise NotImplementedError()


    @property
    def op_str(self):
        return self._value


class ProductionComponent(PyRuleValue):
    """ Pairs a an operator with some bindings """

    def __init__(self, op_str, params, type_str=None, **kwargs):
        super().__init__(op_str, params=params, type_str=type_str, **kwargs)

    def __call__(self):
        raise NotImplementedError()


    @property
    def op(self):
        return self._value

    @property
    def var_set(self):
        obj = super(ProductionComponent, self).var_set
        for p in self._vars:
            if isinstance(p, PyRuleValue):
                tempobj = p.var_set
                obj['in'].update(tempobj['in'])
                obj['out'].update(tempobj['out'])
        return obj


    def __refine_op_func(self, op_str):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        raise NotImplementedError()

    def pprint(self, **kwargs):
        op_fix = [0 if len(self._vars) < 2 else 1][0]
        return PrU.print_operator(self, op_fix=op_fix, **kwargs)

    def copy(self):
        raise NotImplementedError()

    def to_sentence(self, target=None):
        raise NotImplementedError()

    def verify(self):
        """ Complains if the operator is not a defined Operator Enum """
        raise NotImplementedError()


class ProductionContainer(PyRuleValue):

    def __init__(self, clauses, params=None, type_str=STATEMENT_S):
        super().__init__(clauses, params=params, type_str=type_str)

    def __len__(self):
        return len(self.clauses)

    def __call__(self, ctx):
        assert(isinstance(ctx, dict))
        for x in self.clauses:
            ctx[x._rebind._value] = x(ctx)

        return ctx

    def __iter__(self):
        for x in self.clauses:
            yield x


    @property
    def clauses(self):
        return self._value

    @property
    def var_set(self):
        """ Return a set of all bindings this container utilizes """
        # ie: Query(a.b.$x? a.q.$w?).var_set -> {'in': [], 'out': [x,w]}
        # Action(+(a.b.$x), -(a.b.$w)).var_set -> {'in': [x,w], 'out': []}
        obj = super(ProductionContainer, self).var_set
        for p in self.clauses:
            if isinstance(p, PyRuleValue):
                tempobj = p.var_set
                obj['in'].update(tempobj['in'])
                obj['out'].update(tempobj['out'])
        return obj


    def to_sentences(self, target=None):
        return [x.to_sentence() for x in self.clauses]

    def verify(self):
        for x in self.clauses:
            x.verify()

    def expand_bindings(self, bindings):
        raise NotImplementedError()

    def copy(self):
        raise NotImplementedError()

    def pprint(self, **kwargs):
        return PrU.print_container(self, join_str="\n", **kwargs)
