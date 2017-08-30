from enum import Enum
from collections import namedtuple
import IPython

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
    

def expandFact(factString, bindings):
    output = []

    for x in factString:
        if isinstance(x, Bind) and x.value in bindings:
            retrieved = bindings[x.value]
        elif x.get_meta_eval(META_OP.BIND) and x._value in bindings:
            retrieved = bindings[x._value]
        else:
            retrieved = [x]
            
        if retrieved[0].is_root():
            output += retrieved[1:]
        else:
            output += retrieved
    return output
            
        
