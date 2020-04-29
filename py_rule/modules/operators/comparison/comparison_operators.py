"""
Definitions of initial Comparison operators
"""
import re

from py_rule.abstract.comparison import CompOp


class EQ(CompOp):
    def __init__(self):
        super().__init__()
        # type sig: a -> a -> bool

    def __call__(self, a, b, data=None, engine=None):
        return a == b


class NEQ(CompOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a != b


class RegMatch(CompOp):
    def __init__(self):
        super().__init__()
        # type sig: a -> a -> bool

    def __call__(self, a, b, data=None, engine=None):
        return re.search(b, a)


class ELEM(CompOp):
    def __init__(self):
        super().__init__()

    def __call__(self, a, b, data=None, engine=None):
        return a in b
