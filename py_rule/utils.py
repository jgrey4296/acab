""" Cross-module utilities for the rule engines """
from enum import Enum
from random import choice

ROOT_S = "__root"
TYPE_DEC_S = "typedec"
TYPE_DEF_S = "typedef"
BIND_S = "bind"
VALUE_TYPE_S = "value_type"
CONSTRAINT_S = "constraints"
OPERATOR_S = "operator"

#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX')
EXOP_lookup = {
    EXOP.DOT : ".",
    EXOP.EX : "!",
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

def build_rebind_dict(formal, usage):
    """ Build a dictionary for action macro expansion,
    to swap internal formal params for provided usage params """
    assert(all(['bind' in x._data for x in formal]))
    fvals = [x.value for x in formal]
    return dict(zip(fvals, usage))

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
