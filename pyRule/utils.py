from enum import Enum
from collections import namedtuple


EXOP = Enum('EXOP', 'DOT EX ROOT')
COMP = Enum('Comp_ops','LT GT NE EQ')

Bind = namedtuple("Bind","value")
Comparison = namedtuple("Comparison","op value bind")

#todo: split comps into alpha and beta
QueryComponent = namedtuple("QueryComponent","op value bind comps")
Clause = namedtuple("Clause","components negated")
