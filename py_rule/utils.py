""" Cross-module utilities for the rule engines """
from enum import Enum
from collections import namedtuple
from random import choice
from py_rule.abstract.sentence import Sentence
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

def expandFact(factString, bindings):
    """ Given a list of fact components, and a dictionary of bindings,
    reify the fact, using those bindings.
    ie: .a.b.$x with {x: blah} => .a.b.blah
    """
    assert(isinstance(bindings, dict))
    assert(isinstance(factString, Sentence))
    output = []

    for x in factString:
        if x._data['bind'] and x._value in bindings:
            retrieved = bindings[x._value]
        else:
            #early exit if a plain node
            output.append(x)
            continue

        if isinstance(retrieved, Sentence) and len(retrieved) > 1:
            output += retrieved._words
            continue
        else:
            retrieved = retrieved._words[0]

        if retrieved._data['bind']:
            copyNode = x.copy()
            copyNode._value = retrieved._value
            output.append(copyNode)
        else:
            copyNode = x.copy()
            copyNode._value = retrieved._value
            copyNode._data['bind'] = False
            output.append(copyNode)
    return Sentence(output)

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

def has_equivalent_vars_pred(node):
    """ A Predicate to use with Trie.get_nodes
    Finds nodes with multiple vars as children that can be merged """
    if node._op == EX_OP.ROOT:
        return False
    var_children = [x for x in node._children.values() if x.is_var]
    return len(var_children) > 1
