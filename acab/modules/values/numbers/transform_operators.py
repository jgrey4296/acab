from random import uniform
from math import floor
from re import sub

from acab.abstract.core.production_abstractions import ProductionOperator
from acab.modules.semantics.util import SemanticOperatorWrapDecorator

class AddOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, left, right, data=None):
        return left + right


class SubOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, b, data=None):
        return a - b


class MulOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, b, data=None):
        return a * b


class DivOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, b, data=None):
        return a / b


class RandOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a=0, b=1, data=None):
        """ Uniform Rand between a=0 and b=1 """
        return uniform(a, b)


class RemainOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, b, data=None):
        # divide and get remainder?
        raise NotImplementedError()


class RoundOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, data=None):
        # round to integer
        return floor(a)


class NegOp(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, data=None):
        # invert the number
        return -a
