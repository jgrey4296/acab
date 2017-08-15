from enum import Enum
from collections import namedtuple

#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX ROOT')
EXOP_lookup = {
    EXOP.DOT : ".",
    EXOP.EX : "!",
    EXOP.ROOT : "||"
    }

META_OP = Enum("Meta Ops", "ORDER BIND COMP NEGATED")


#Basic Data Structures
class Bind: #pylint: disable=too-few-public-methods 
    """ Simple holder to designate a binding action """
    def __init__(self, v):
        self.value = v
        
    def __repr__(self):
        return "$" + self.value
    
