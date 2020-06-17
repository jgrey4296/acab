from random import uniform, sample, randint
from math import floor
from re import sub

from acab.abstract.transform import TransformOp


class AddOp(TransformOp):
    def __init__(self):
        super().__init__()
        # type sig: num -> num -> num
    def __call__(self, left, right, data=None, engine=None):
        return left + right


class SubOp(TransformOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a - b


class MulOp(TransformOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a * b


class DivOp(TransformOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a / b


class RandOp(TransformOp):
    def __init__(self):
        super().__init__()
        # ts: num -> num -> num
    def __call__(self, a=0, b=1, data=None, engine=None):
        """ Uniform Rand between a=0 and b=1 """
        return uniform(a, b)


class RemainOp(TransformOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        # divide and get remainder?
        raise NotImplementedError()


class RoundOp(TransformOp):
    def __init__(self):
        super().__init__(1)

    def __call__(self, a, data=None, engine=None):
        # round to integer
        return floor(a)


class NegOp(TransformOp):
    def __init__(self):
        super().__init__(1)

    def __call__(self, a, data=None, engine=None):
        # invert the number
        return -a
