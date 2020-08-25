"""
Cross-module utilities for the rule engines
"""
from enum import Enum

from acab.config import AcabConfig
from acab.abstract.printing import util as PrU

util = AcabConfig.Get()

OPERATOR_S = util("Parsing.Structure", "OPERATOR_S")
ROOT_S = util("Data.Trie", "ROOT_S")
#Trie exclusion operator:
EXOP = Enum('EXOP', 'DOT EX')

PrU.register_modal({EXOP.DOT : util("WorkingMemory.TrieWM.Parsing", "EXOP.DOT_S"),
                    EXOP.EX : util("WorkingMemory.TrieWM.Parsing", "EXOP.EX_S")})

DEFAULT_NODE_DATA = {
    OPERATOR_S : EXOP[util("WorkingMemory.TrieWM", "DEFAULT_EXOP")]
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
