from enum import Enum
from collections import namedtuple
from random import choice
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
            #early exit if a plain node
            output.append(x)
            continue
        
        if isinstance(retrieved, list) and retrieved[0].is_root():
            output += retrieved[1:]
        elif isinstance(retrieved, list):
            output += retrieved
        elif isinstance(retrieved, Bind):
            copyNode = x.copy()
            copyNode._value = retrieved.value
            output.append(copyNode)
        else:
            copyNode = x.copy()
            copyNode._value = retrieved
            copyNode.set_meta_eval(META_OP.BIND, False)
            output.append(copyNode)
            
    return output
            
        
def build_rebind_dict(formal, usage):
    """ Build a dictionary for action macro expansion,
    to swap internal formal params for proided usage params """
    assert(all([isinstance(x, Bind) for x in formal]))
    fVals = [x.value for x in formal]
    return dict(zip(fVals, usage))

def default_action_policy(pairings):
    #each pairing is of data, rule
    return [choice(pairings)]


