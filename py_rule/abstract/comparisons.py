""" Simple comparison functions to be used in rules """
import logging as root_logger
from enum import Enum
import re
logging = root_logger.getLogger(__name__)

#comparison operators:
COMP = Enum('Comp_ops', 'LT GT NE EQ RE')

class CompOp:
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


class EQ(CompOp):
    def __init__(self):
        super().__init__("==")

    def __call__(self, a, b):
        return a == b


class GT(CompOp):
    def __init__(self):
        super().__init__(">")

    def __call__(self, a, b):
        return a > b


class LT(CompOp):
    def __init__(self):
        super().__init__("<")

    def __call__(self, a, b):
        return a < b


class NEQ(CompOp):
    def __init__(self):
        super().__init__("!=")

    def __call__(self, a, b):
        return a != b


class RegMatch(CompOp):
    def __init__(self):
        super().__init__("~=")

    def __call__(self, a, b):
        return re.search(b, a)


EQ()
GT()
LT()
NEQ()
RegMatch()


class Comparison:
    """ Describe a Comparison of values and maybe a binding """

    def __init__(self, op, value):
        self.op = CompOp.op_list[op]
        self.value = value

    def copy(self):
        return Comparison(self.op, self.value)

    def is_alpha_test(self):
        return self.value is not None

    def is_regex_test(self):
        return self.op is COMP.RE

    def __str__(self):
        if self.is_regex_test():
            val = "/{}/".format(self.value)
        else:
            val = self.value

        retValue = "{} {}".format(str(self.op), str(val))
        return retValue

    def __repr__(self):
        if self.is_regex_test():
            val = "/{}/".format(self.value)
        else:
            val = self.value

        retValue = "Comparison({} {})".format(repr(self.op), repr(val))
        return retValue
