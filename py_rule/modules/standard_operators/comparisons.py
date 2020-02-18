"""
Definitions of initial Comparison operators
"""
from enum import Enum
from py_rule.abstract.comparison import CompOp
import re

COMP = Enum('Comp_ops', 'LT GT NE EQ RE')


class EQ(CompOp):
    def __init__(self):
        super().__init__("==")

    def __call__(self, a, b):
        return a == b


class GT(CompOp):
    def __init__(self):
        super().__init__(">")

    def __call__(self, a, b):
        return a > b


class LT(CompOp):
    def __init__(self):
        super().__init__("<")

    def __call__(self, a, b):
        return a < b


class NEQ(CompOp):
    def __init__(self):
        super().__init__("!=")

    def __call__(self, a, b):
        return a != b


class RegMatch(CompOp):
    def __init__(self):
        super().__init__("~=")

    def __call__(self, a, b):
        return re.search(b, a)


class ELEM(CompOp):
    def __init__(self):
        super().__init__("∈")

    def __call__(self, a, b):
        return a in b

