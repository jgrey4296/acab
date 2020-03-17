""" Simple comparison functions to be used in rules """
import logging as root_logger
from .production_operator import ProductionOperator
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


class Comparison:
    """ Describe a Comparison of values and maybe a binding """

    def __init__(self, op_str, value):
        self._op_str = op_str
        self._value = value

    def __str__(self):
        val = self._value.opless_print()

        retValue = "{} {}".format(str(self._op_str), val)
        return retValue

    def __repr__(self):
        if self.is_regex_test():
            val = "/{}/".format(self._value)
        else:
            val = self._value

        retValue = "Comparison({} {})".format(repr(self._op_str), repr(val))
        return retValue

    def copy(self):
        return Comparison(self._op_str, self._value)

    def is_alpha_test(self):
        return self._value is not None and not self._value._data[BIND_S]

    def is_regex_test(self):
        return self._op_str is "RegMatch"

    def refine_operator(self, op_str):
        self._op_str = op_str

    def __call__(self, node, data=None):
        """ Run a comparison on a node """
        op = CompOp.op_list[self._op_str]
        node_value = node._value
        value = self._value._value
        if data is not None:
            value = data[value]
        return op(node_value, value)

    def to_sentence(self, target):
        """ Create a comparison as a canonical sentence """
        # eg: 20(>30) -> > 20 30 -> bool
        return Sentence([self._op_str, target, self._value])
