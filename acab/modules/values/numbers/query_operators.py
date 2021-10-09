from acab.core.data.production_abstractions import ProductionOperator
from acab.core.decorators.semantics import OperatorArgUnWrap, OperatorResultWrap


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
