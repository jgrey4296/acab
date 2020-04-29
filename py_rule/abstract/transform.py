""" Simple Transform functions to be used in rules """
import logging as root_logger

from py_rule import util
from py_rule.abstract.printing import util as PrU

from . import production_operator as PO
from .node import PyRuleNode
from .sentence import Sentence

logging = root_logger.getLogger(__name__)


class TransformOp(PO.ProductionOperator):
    op_list = {}

    def __init__(self, num_params=2, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(num_params=num_params, infix=False)

        if self.op_str not in TransformOp.op_list:
            TransformOp.op_list[self.op_str] = self


    def __call__(self, a, b, data=None, engine=None):
        raise NotImplementedError("Abstract method needs to be implemented")


class TransformComponent(PO.ProductionComponent):
    """ Superclass of OperatorTransform. Holds an Operator """
    def __init__(self, op_str, params, rebind=None, type_str=None):
        super(TransformComponent, self).__init__(op_str, params, op_class=TransformOp)
        self._rebind = rebind

    @property
    def var_set(self):
        obj = super(TransformComponent, self).var_set
        in_set = obj['out']
        out_set = set()
        if self._rebind is not None:
            out_set.update(self._rebind.var_set['out'])
        return {'in' : in_set, 'out': out_set}


    def set_rebind(self, bind):
        """ Set this transform to rebind its result to a different variable """
        assert(isinstance(bind, PyRuleNode))
        assert(util.AT_BIND_S not in bind._data)
        self._rebind = bind
        return self

    def copy(self):
        copied = TransformComponent(self.op,
                                    self._vars,
                                    rebind=self._rebind,
                                    type_str=self.type)
        return copied

    def to_sentence(self, target=None):
        head = PyRuleNode(self.op, {util.OPERATOR_S : self})
        return Sentence([head] + self._vars[:] + [self._rebind])


class Transform(PO.ProductionContainer):
    """ Holds a number of separate transform
    operators together to apply to a binding set
    """

    # have min and max bounds
    def __init__(self, clauses, type_str=util.TRANSFORM_S, **kwargs):
        assert(all([isinstance(x, TransformComponent) for x in clauses]))
        super(Transform, self).__init__(clauses, type_str=type_str, **kwargs)

    @property
    def var_set(self):
        var_set = {'in': set(), 'out': set()}
        for clause in self.clauses:
            for word in clause._vars:
                if word.is_var:
                    var_set['in'].add(word._value)
            var_set['out'].add(x._rebind._value)

        return var_set

    def copy(self):
        return Transform([x.copy() for x in self.clauses], type_str=self.type)

    def __call__(self, ctx, engine):
        assert(isinstance(ctx, dict))
        for x in self.clauses:
            ctx[x._rebind._value] = x(ctx, engine)

        return ctx
