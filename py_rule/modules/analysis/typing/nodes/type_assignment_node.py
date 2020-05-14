import logging as root_logger

import py_rule.error.type_exceptions as te
from py_rule.modules.analysis.typing import util
from .typed_node import MonoTypedNode

logging = root_logger.getLogger(__name__)


class TypeAssignmentTrieNode(MonoTypedNode):
    """ A Node in the Type Assignment Trie.
    Used in type inference.
    Enables linking with variable type trie """

    def __init__(self, value, _type=None, var_node=None):
        assert(var_node is None or isinstance(var_node, MonoTypedNode))
        super().__init__(value, _type=_type)
        self._var_node = var_node

    def update(self, node, lookup):
        """ Post-addition update method.
        links self to a lookup-trie node if self is a variable """
        logging.debug("Node: {} updating with {}".format(self._value,
                                                         str(node)))
        # apply type if necessary
        self.type_match_wrapper(node)
        if not (self.is_var == node.is_var):
            # complain if var status doesn't match
            raise te.TypeVariableConflictException(self)

        if self.is_var and self._var_node is None:
            # if var, connect to var type trie
            self._var_node = lookup.add([self._value], [])
            self._var_node.add_node(self)

    def clear_var_node(self):
        self._var_node = None
