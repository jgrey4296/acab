""" Simple Transform functions to be used in rules """
import logging as root_logger

from py_rule import util
from py_rule.abstract.printing import util as PrU

from . import production_operator as PO
from .node import PyRuleNode
from .sentence import Sentence

logging = root_logger.getLogger(__name__)


class TransformOp(PO.ProductionOperator):
    op_list = {1 : {},
               2 : {},
               3 : {}}

    def __init__(self, num_params=2, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(num_params=num_params, infix=False)

        if self.op_str not in TransformOp.op_list[num_params]:
            TransformOp.op_list[num_params][self.op_str] = self

    def __call__(self, a, b):
        raise NotImplementedError("Abstract method needs to be implemented")


class TransformComponent(PO.ProductionComponent):
    """ Superclass of OperatorTransform. Holds an Operator """
    def __init__(self, op_str, params, rebind=None, type_str=None):
        assert(not any([util.AT_BIND_S in x._data for x in params]))
        super(TransformComponent, self).__init__(op_str, params)
        self._rebind = rebind

    def __refine_op_func(self, op_str):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        assert(op_str in TransformOp.op_list)
        self._value = op_str

    def __call__(self, ctx):
        op_func = TransformOp.op_list[len(self._vars)][self.op]
        params = [ctx[y._value] if y.is_var else y._value for y in self._vars]
        return op_func(*params, ctx)

    def verify(self):
        """ Complains if the operator is not a defined Operator Enum """
        if self.op not in TransformOp.op_list[len(self._vars)]:
            raise SyntaxError("Unknown Op: {}".format(self.op))

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

    def var_set(self):
        obj = super(TransformComponent, self).var_set()
        in_set = obj['out']
        out_set = set()
        if self._rebind is not None:
            out_set.update(self._rebind.var_set()['out'])
        return {'in' : in_set, 'out': out_set}


class Transform(PO.ProductionContainer):
    """ Holds a number of separate transform
    operators together to apply to a binding set
    """

    # have min and max bounds
    def __init__(self, clauses, type_str=util.TRANSFORM_S, **kwargs):
        assert(all([isinstance(x, TransformComponent) for x in clauses]))
        super(Transform, self).__init__(clauses, type_str=type_str, **kwargs)

    def copy(self):
        return Transform([x.copy() for x in self.clauses], type_str=self.type)

    def get_input_requirements(self):
        # return the set of input bound names
        return [y._value for x in self._clauses for y in x._vars if y.is_var]

    def get_output_spec(self):
        # return the set of output bound names
        return [x._rebind._value for x in self._clauses]

