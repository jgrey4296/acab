from enum import Enum
from collections import namedtuple
import pyRule.Comparisons as C

#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX ROOT')
EXOP_lookup = {
    EXOP.DOT : ".",
    EXOP.EX : "!",
    EXOP.ROOT : "||"
    }
#transform operators:
TROP = Enum("Transform_ops", "ADD SUB MUL DIV RAND RANGE REMAIN ROUND")

META_OP = Enum("Meta Ops", "ORDER BIND COMP NEGATED")

#Action operators:
ACTS = Enum('Action_ops', 'ASSERT RETRACT PRINT CUSTOM')

#Basic Data Structures
class Bind: #pylint: disable=too-few-public-methods 
    """ Simple holder to designate a binding action """
    def __init__(self, v):
        self.value = v
        
    def __repr__(self):
        return "$" + self.value





TransformComponent = namedtuple("TransformComponent","op source val bind rebind")
Transform = namedtuple("Transform","components")


Action = namedtuple("Action", "op values")
