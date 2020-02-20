""" Cross-module utilities for the rule engines """
from enum import Enum
from random import choice


# Globally constant strings:
BIND_S       = "bind"
CONSTRAINT_S = "constraints"
#Use to signify a decimal, eg: 34d423 = 34.423
DECIMAL_S    = "d"
FLOAT_S      = "float"
INT_S        = "int"
NAME_S       = "name"
OPERATOR_S   = "operator"
OPT_S        = "opt"
PATTERN_S    = "pattern"
REGEX_S      = "regex"
ROOT_S       = "__root"
STRING_S     = "string"
SUB_S        = '-'
UNDERSCORE_S = "_"
VALUE_S      = "value"
VALUE_TYPE_S = "value_type"


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
