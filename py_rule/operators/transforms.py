from pyrule.abstract import transforms
from random import uniform, sample, randint
from math import floor
from re import sub
from enum import Enum

TROP = Enum("Transform_ops", "ADD SUB MUL DIV RAND REMAIN ROUND NEG REGEX FORMAT SELECT SELECT_ALL")


class AddOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("+")

    def __call__(self, a, b, data):
        return a + b


class SubOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("-")

    def __call__(self, a, b, data):
        return a - b


class MulOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("*")

    def __call__(self, a, b, data):
        return a * b


class DivOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("/")

    def __call__(self, a, b, data):
        return a / b


class RandOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("<->")

    def __call__(self, a=0, b=1, data=None):
        """ Uniform Rand between a=0 and b=1 """
        return uniform(a, b)


class RemainOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("%")

    def __call__(self, a, b, data):
        # divide and get remainder?
        raise Exception("Not implemented yet")


class RoundOp(transforms.TransformOp):
    def __init__(self):
        super().__init__('_', 1)

    def __call__(self, a, data):
        # round to integer
        return floor(a)


class NegOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("-", 1)

    def __call__(self, a, data):
        # invert the number
        return -a


class RegexOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("~=", 3)

    def __call__(self, a, b, replacement, data):
        """ Substitute a pattern with a value from passed in data
        a : the replacement
        b: the pattern

        """
        return sub(b, replacement, a)


class FormatOp(transforms.TransformOp):
    def __init__(self):
        super().__init__("~{}", 1)

    def __call__(self, a, data):
        """ Use str.format variant with a data dictionary
        Replaces variables in the string with bound values
        """
        return a.format(**data)


# Construct the operators:
AddOp()
SubOp()
MulOp()
DivOp()
RandOp()
RemainOp()
RoundOp()
NegOp()
RegexOp()
FormatOp()