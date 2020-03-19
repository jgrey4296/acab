""" Cross-module utilities for the rule engines """
from enum import Enum
from random import choice


# Globally constant strings:

# Basic constants
NAME_S       = "name"
BIND_S       = "bind"
AT_BIND_S    = "at_bind_"
CONSTRAINT_S = "constraints"
OPERATOR_S   = "operator"
OPT_S        = "opt"
VALUE_S      = "value"
VALUE_TYPE_S = "value_type"
SEN_S        = "sentence"

# Core Value Types
FLOAT_S      = "float"
INT_S        = "int"
#Use to signify a decimal, eg: 34d423 = 34.423
DECIMAL_S    = "d"

REGEX_S      = "regex"
STRING_S     = "string"
PATTERN_S    = "pattern"

# Trie Root Node Name
ROOT_S       = "__root"

# Operator Constants
SUB_S        = '-'
UNDERSCORE_S = "_"

# Rule Constants
LAYER_QUERY_RULE_BIND_S = "rule"
RULE_S = "rule"

# Higher Level Structure Heads
MACRO_S     = "μ"
STRUCTURE_S = "σ"
FUNC_S      = "λ"

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
