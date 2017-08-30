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
    

def expandFact(factString, bindings):
    output = []
    if isinstance(factString[0], Bind) and factString[0].value in bindings:
        output += bindings[factString[0].value]
    else:
        output.append(factString[0])

    for x in factString[1:]:
        if x.get_meta_eval(META_OP.BIND) and x._value in bindings:
            retrieved = bindings[x._value]
            if retrieved[0].is_root():
                output += retrieved[1:]
            else:
                output += retrieved
        else:
            output.append(x)
    return output
            
        
