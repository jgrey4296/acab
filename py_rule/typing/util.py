import py_rule.utils as utils
from py_rule.trie.nodes.trie_node import TrieNode
import logging as root_logger
logging = root_logger.getLogger(__name__)


def has_equivalent_vars_pred(node):
    """ A Predicate to use with Trie.get_nodes
    Finds nodes with multiple vars as children that can be merged """
    if node._value == utils.ROOT_S:
        return False
    var_children = [x for x in node._children.values() if x._is_var]
    return len(var_children) > 1


def is_var(node):
    return node._data[utils.BIND_S]
