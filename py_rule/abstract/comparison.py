""" Simple comparison functions to be used in rules """
import logging as root_logger
from .production_operator import ProductionOperator, ProductionComponent
from py_rule.util import BIND_S
from .sentence import Sentence
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
        if self._op_str not in CompOp.op_list:
            CompOp.op_list[self._op_str] = self

    def __call__(self, a, b):
        raise NotImplementedError()

    def __str__(self):
        return self._op_str

    def __repr__(self):
        return "CompOp({})".format(str(self))


class Comparison(ProductionComponent):
    """ Describe a Comparison of values and maybe a binding """

    def __init__(self, op_str, param):
        super(Comparison, self).__init__(op_str, [param])

    def __str__(self):
        val = self._params[0].opless_print()

        retValue = "{} {}".format(str(self._op), val)
        return retValue

    def __repr__(self):
        if self.is_regex_test():
            val = "/{}/".format(self._value)
        else:
            val = self._value

        retValue = "Comparison({} {})".format(repr(self._op), repr(val))
        return retValue

    def __call__(self, node, data=None):
        """ Run a comparison on a node """
        op = CompOp.op_list[self._op]
        node_value = node._value
        value = self._params[0]._value
        if data is not None:
            value = data[value]
        return op(node_value, value)

    def copy(self):
        return Comparison(self._op, self._value)

    def is_alpha_test(self):
        return bool(self._params) and not self._params[0]._data[BIND_S]

    def is_regex_test(self):
        return self._op is "RegMatch"

    def refine_operator(self, op_str):
        self._op = op_str

    def to_sentence(self, target):
        """ Create a comparison as a canonical sentence """
        # eg: 20(>30) -> > 20 30 -> bool
        return Sentence([self._op, target, self._value])
