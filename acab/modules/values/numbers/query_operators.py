from acab.core.value.instruction import ProductionOperator
from acab.core.util.decorators.semantics import OperatorArgUnWrap, OperatorResultWrap


class GT(ProductionOperator):
    @OperatorArgUnWrap
    @OperatorResultWrap
    def __call__(self, a, b, data=None):
        return a > b


class LT(ProductionOperator):
    @OperatorArgUnWrap
    @OperatorResultWrap
    def __call__(self, a, b, data=None):
        return a < b
