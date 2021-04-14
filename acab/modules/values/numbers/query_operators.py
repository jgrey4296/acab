from acab.abstract.core.production_abstractions import ProductionOperator
from acab.modules.semantics.util import SemanticOperatorWrapDecorator


class GT(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, b, data=None):
        return a > b


class LT(ProductionOperator):
    @SemanticOperatorWrapDecorator
    def __call__(self, a, b, data=None):
        return a < b
