""" Simple comparison functions to be used in rules """
import logging as root_logger

from py_rule.util import BIND_S, OPERATOR_S
from py_rule.abstract.printing import util as PrU

from .production_operator import ProductionOperator, ProductionComponent
from .sentence import Sentence
from .node import PyRuleNode

logging = root_logger.getLogger(__name__)


class CompOp(ProductionOperator):
    """ Superclass for Comparisons.
    Instantiation of subclasses auto-registers
    the comparison into CompOp.op_list with an operator string
    """
    op_list = {}

    def __init__(self, num_params=2, infix=False):
        # Registers self with class name,
        # DSL later binds to an operator
        super().__init__(num_params=num_params, infix=False)

        if self.op_str not in CompOp.op_list:
            CompOp.op_list[self.op_str] = self

    def __call__(self, a, b):
        raise NotImplementedError()


class Comparison(ProductionComponent):
    """ Describe a Comparison of values and maybe a binding """

    def __init__(self, op_str, param, type_str=None):
        if not isinstance(param, list):
            param = [param]
        super(Comparison, self).__init__(op_str,
                                         param,
                                         type_str=type_str)

    def __call__(self, node, data=None):
        """ Run a comparison on a node """
        op = CompOp.op_list[self.op]
        node_value = node._value
        value = self._vars[0]._value
        if data is not None:
            value = data[value]
        return op(node_value, value)

    def __refine_op_func(self, op_str):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        assert(op_str in CompOp.op_list)
        self._value = op_str

    def is_alpha_test(self):
        """ Return boolean of if test does not rely on other bindings """
        return bool(self._vars) and not self._vars[0].is_var

    def is_regex_test(self):
        """ Return boolean of if test is a regular expression test """
        return self.op == "RegMatch"

    def to_sentence(self, target=None):
        """ Create a comparison as a canonical sentence """
        # eg: 20(>30) -> > 20 30 -> bool
        head = PyRuleNode(self.op, {OPERATOR_S : self})
        if target is None:
            return Sentence([head] + self._vars)
        assert(isinstance(target, PyRuleNode))
        return Sentence([head, target] + self._vars)

    def var_set(self):
        obj = super(Comparison, self).var_set()
        return {'in': obj['in'].union(obj['out']), 'out': set()}
