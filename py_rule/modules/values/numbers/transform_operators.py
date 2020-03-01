from py_rule.abstract.transform import TransformOp
from random import uniform, sample, randint
from math import floor
from re import sub


class AddOp(TransformOp):
    def __init__(self):
        super().__init__("+")
        # type sig: num -> num -> num
    def __call__(self, a, b, data):
        return a + b


class SubOp(TransformOp):
    def __init__(self):
        super().__init__("-")

    def __call__(self, a, b, data):
        return a - b


class MulOp(TransformOp):
    def __init__(self):
        super().__init__("*")

    def __call__(self, a, b, data):
        return a * b


class DivOp(TransformOp):
    def __init__(self):
        super().__init__("/")

    def __call__(self, a, b, data):
        return a / b


class RandOp(TransformOp):
    def __init__(self):
        super().__init__("<->")
        # ts: num -> num -> num
    def __call__(self, a=0, b=1, data=None):
        """ Uniform Rand between a=0 and b=1 """
        return uniform(a, b)


class RemainOp(TransformOp):
    def __init__(self):
        super().__init__("%")

    def __call__(self, a, b, data):
        # divide and get remainder?
        raise NotImplementedError()


class RoundOp(TransformOp):
    def __init__(self):
        super().__init__('_', 1)

    def __call__(self, a, data):
        # round to integer
        return floor(a)


class NegOp(TransformOp):
    def __init__(self):
        super().__init__("-", 1)

    def __call__(self, a, data):
        # invert the number
        return -a
