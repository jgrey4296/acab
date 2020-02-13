from enum import Enum
from pyrule.abstract import comparisons
import re

COMP = Enum('Comp_ops', 'LT GT NE EQ RE')


class EQ(comparisons.CompOp):
    def __init__(self):
        super().__init__("==")

    def __call__(self, a, b):
        return a == b


class GT(comparisons.CompOp):
    def __init__(self):
        super().__init__(">")

    def __call__(self, a, b):
        return a > b


class LT(comparisons.CompOp):
    def __init__(self):
        super().__init__("<")

    def __call__(self, a, b):
        return a < b


class NEQ(comparisons.CompOp):
    def __init__(self):
        super().__init__("!=")

    def __call__(self, a, b):
        return a != b


class RegMatch(comparisons.CompOp):
    def __init__(self):
        super().__init__("~=")

    def __call__(self, a, b):
        return re.search(b, a)


class ELEM(comparisons.CompOp):
    def __init__(self):
        super().__init__("∈")

    def __call__(self, a, b):
        return a in b


# Actually Create the Operators:
EQ()
GT()
LT()
NEQ()
RegMatch()
ELEM()
