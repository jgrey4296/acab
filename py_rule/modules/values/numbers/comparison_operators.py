from py_rule.abstract.comparison import CompOp


class GT(CompOp):
    def __init__(self):
        super().__init__()
    def __call__(self, a, b):
        return a > b


class LT(CompOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b):
        return a < b
