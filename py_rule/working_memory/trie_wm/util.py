"""
Cross-module utilities for the rule engines
"""
from enum import Enum

from py_rule import util
from py_rule.abstract.printing import util as PrU

ROOT_S       = util.ROOT_S
BIND_S       = util.BIND_S
AT_BIND_S    = util.AT_BIND_S
VALUE_S      = util.VALUE_S
NAME_S       = util.NAME_S
STRING_S     = util.STRING_S
VALUE_TYPE_S = util.VALUE_TYPE_S
CONSTRAINT_S = util.CONSTRAINT_S
OPERATOR_S   = util.OPERATOR_S
REGEX_S      = util.REGEX_S
RULE_S       = util.RULE_S
NEGATION_S   = util.NEGATION_S
FALLBACK_S   = util.FALLBACK_S
QUERY_S      = util.QUERY_S
TRANSFORM_S  = util.TRANSFORM_S
ACTION_S     = util.ACTION_S


NODE_S        = "node"
COMP_S        = 'comparison'
MAIN_CLAUSE_S = "main_clause"
LEFT_S        = "left"
RIGHT_S       = "right"
SOURCE_S      = "source"
REPLACE_S     = "replace"
TARGET_S      = "target"
ANNOTATION_S  = "annotations"
RULE_NAME_S   = "rule_name"
CONDITION_S   = "conditions"
ACTION_VAL_S  = "action_values"


#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX')

PrU.setup_modal_lookups({EXOP.DOT : ".", EXOP.EX : "!" })

DEFAULT_NODE_DATA = {
    OPERATOR_S : EXOP.DOT,
    }


# Utility Funtions:
def node_is_exclusive(node):
    """ Checks for the exclusion operator in this node """
    return node._data[util.OPERATOR_S] is EXOP.EX

def node_looks_exclusive(node):
    """ Checks for implicit exclusivity by having 0 or 1 children """
    return len(node) <= 1

def exclusion_matches(contexts, a, b, data):
    """ Compare the EXOP of a node, with whether that exop
        is in the children of the other node/parent """
    logging.info("Running exclusion match test: {} {}".format(str(a), str(b)))
    assert(isinstance(a, TrieNode))
    assert(isinstance(b, TrieNode))
    result = node_is_exclusive(a) and not node_looks_exclusive(b)
    if result:
        logging.info("C: {}, Node: {}, {}".format(node_is_exclusive(a),
                                                  node_is_exclusive(b), str(b)))
        logging.info("Mismatch EX num")

    # Don't break out of tests unless this test failed:
    return False, result
