from enum import Enum
from collections import namedtuple

#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX ROOT')
#comparison operators:
COMP = Enum('Comp_ops','LT GT NE EQ')
#transform operators:
TROP = Enum("Transform_ops", "ADD SUB MUL DIV RAND RANGE REMAIN ROUND")

#Action operators:
ACTS = Enum('Action_ops', 'ASSERT RETRACT PRINT CUSTOM')

#Basic Data Structures
class Bind: #pylint: disable=too-few-public-methods 
    """ Simple holder to designate a binding action """
    def __init__(self, v):
        self.value = v

    def __str__(self):
        return "$" + self.value

Comparison = namedtuple("Comparison","op value bind")

#todo: split comps into alpha and beta
QueryComponent = namedtuple("QueryComponent","op value bind comps")
Clause = namedtuple("Clause","components negated")


TransformComponent = namedtuple("TransformComponent","op source val bind rebind")
Transform = namedtuple("Transform","components")


Action = namedtuple("Action", "op values")
