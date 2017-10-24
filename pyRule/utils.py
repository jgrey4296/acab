""" Cross-module utilities for the rule engines """
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

#Meta Operators.
#BIND: Designates a node's value as a binding variable
#COMP: Designates the node to have comparisons to run on query eval
#RULE: Designates a node to have a meta-leaf of a rule
#RULEBIND: Designates the node has the eval property of testing for,
##then binding, a meta-leaf rule
META_OP = Enum("Meta Ops", "BIND COMP RULE RULEBIND")


#Mutability. Core are authored and immutable, required for intelligibility
#Penumbra are generated, social, and mutable
#for use in query clauses, and actions
MUTABLE = Enum('Mutablility', "CORE PENUMBRA")

#Basic Data Structures
class Bind:
    """ Simple holder to designate a binding action """
    #pylint: disable=too-few-public-methods
    def __init__(self, v):
        self.value = v

    def __repr__(self):
        return "$" + self.value


def expandFact(factString, bindings):
    """ Given a list of fact components, and a dictionary of bindings,
    reify the fact, using those bindings.
    ie: .a.b.$x with {x: blah} => .a.b.blah 
    """
    assert(isinstance(bindings, dict))
    assert(isinstance(factString, list))
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
    to swap internal formal params for provided usage params """
    assert(all([isinstance(x, Bind) for x in formal]))
    fVals = [x.value for x in formal]
    return dict(zip(fVals, usage))

def default_action_policy(pairings):
    """ A Simple random selection policy """
    #each pairing is of data, rule
    return [choice(pairings)]
