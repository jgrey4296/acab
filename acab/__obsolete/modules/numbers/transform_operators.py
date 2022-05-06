from random import uniform
from math import floor
from re import sub

from acab.core.value.instruction import ProductionOperator
from acab.core.util.decorators.semantic import OperatorValueWrap


class AddOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, left, right, data=None):
        return left + right


class SubOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, a, b, data=None):
        return a - b


class MulOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, a, b, data=None):
        return a * b


class DivOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, a, b, data=None):
        return a / b


class RandOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, a=0, b=1, data=None):
        """ Uniform Rand between a=0 and b=1 """
        return uniform(a, b)


class RemainOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, a, b, data=None):
        # divide and get remainder?
        raise NotImplementedError()


class RoundOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, a, data=None):
        # round to integer
        return floor(a)


class NegOp(ProductionOperator):
    @OperatorValueWrap
    def __call__(self, a, data=None):
        # invert the number
        return -a
