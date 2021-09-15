from acab.abstract.core.production_abstractions import ProductionOperator
from acab.abstract.decorators.semantics import OperatorArgUnWrap, OperatorResultWrap


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
