from acab.abstract.rule.production_abstractions import ProductionOperator


class GT(ProductionOperator):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a > b


class LT(ProductionOperator):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a < b
