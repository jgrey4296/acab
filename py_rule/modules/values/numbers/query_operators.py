from py_rule.abstract.query import QueryOp


class GT(QueryOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a > b


class LT(QueryOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a < b
