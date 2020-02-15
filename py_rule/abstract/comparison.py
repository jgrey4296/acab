""" Simple comparison functions to be used in rules """
import logging as root_logger
from py_rule.utils import BIND_S
logging = root_logger.getLogger(__name__)


class CompOp:
    """ Superclass for Comparisons.
    Instantiation of subclasses auto-registers
    the comparison into CompOp.op_list with an operator string
    """
    op_list = {}

    def __init__(self, op_str):
        self._op_str = op_str
        CompOp.op_list[str(self)] = self

    def __call__(self, a, b):
        raise Exception("Abstract CompOp")

    def __str__(self):
        return self._op_str

    def __repr__(self):
        return "CompOp({})".format(str(self))


class Comparison:
    """ Describe a Comparison of values and maybe a binding """

    def __init__(self, op, value):
        self._op = CompOp.op_list[op]
        self._value = value

    def __str__(self):
        val = self._value.opless_print()

        retValue = "{} {}".format(str(self._op), val)
        return retValue

    def __repr__(self):
        if self.is_regex_test():
            val = "/{}/".format(self._value)
        else:
            val = self._value

        retValue = "Comparison({} {})".format(repr(self._op), repr(val))
        return retValue

    def copy(self):
        return Comparison(self._op, self._value)

    def is_alpha_test(self):
        return self._value is not None and not self._value._data[BIND_S]

    def is_regex_test(self):
        return self._op is CompOp.op_list["~="]
