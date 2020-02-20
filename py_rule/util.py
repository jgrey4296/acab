""" Cross-module utilities for the rule engines """
from enum import Enum
from random import choice

# Globally constant strings:

ROOT_S = "__root"
BIND_S = "bind"
VALUE_S = "value"
NAME_S = "name"
STRING_S = "string"
FLOAT_S = "float"
#Use to signify a decimal, eg: 34d423 = 34.423
DECIMAL_S = "d"
INT_S = "int"
VALUE_TYPE_S = "value_type"
CONSTRAINT_S = "constraints"
OPERATOR_S = "operator"
OPT_S = "opt"
REGEX_S = "regex"
UNDERSCORE_S = "_"

PATTERN_S = "pattern"

def build_rebind_dict(formal, usage):
    """ Build a dictionary for action macro expansion,
    to swap internal formal params for provided usage params """
    assert(all([BIND_S in x._data for x in formal]))
    fvals = [x._value for x in formal]
    return dict(zip(fvals, usage))

def has_equivalent_vars_pred(node):
    """ A Predicate to use with Trie.get_nodes
    Finds nodes with multiple vars as children that can be merged """
    if node._op == EX_OP.ROOT:
        return False
    var_children = [x for x in node._children.values() if x._is_var]
    return len(var_children) > 1
