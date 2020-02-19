"""
Cross-module utilities for the rule engines
"""
from enum import Enum

NODE_S        = "node"
COMP_S        = 'comparison'
NOT_S         = 'NOT'
TYPE_DEC_S    = "type_dec"
TYPE_DEF_S    = "type_def"
FALLBACK_S    = "fallback_bindings"
MAIN_CLAUSE_S = "main_clause"
LEFT_S        = "left"
RIGHT_S       = "right"
SOURCE_S      = "source"
REPLACE_S     = "replace"
TRANSFORM_S   = "transform"
TARGET_S      = "target"
ANNOTATION_S  = "annotations"
RULENAME_S    = "rule_name"
CONDITION_S   = "conditions"
ACTION_S      = "action"
ACTION_VAL_S  = "action_values"
TAG_S         = "tag"

#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX')
EXOP_lookup = {
    EXOP.DOT : ".",
    EXOP.EX : "!",
    }

# Utility Funtions:
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
