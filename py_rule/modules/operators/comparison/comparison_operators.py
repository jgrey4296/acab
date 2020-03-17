"""
Definitions of initial Comparison operators
"""
from py_rule.abstract.comparison import CompOp
import re


class EQ(CompOp):
    def __init__(self):
        super().__init__()
        # type sig: a -> a -> bool

    def __call__(self, a, b):
        return a == b


class NEQ(CompOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b):
        return a != b


class RegMatch(CompOp):
    def __init__(self):
        super().__init__()
        # type sig: a -> a -> bool

    def __call__(self, a, b):
        return re.search(b, a)


class ELEM(CompOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b):
        return a in b

