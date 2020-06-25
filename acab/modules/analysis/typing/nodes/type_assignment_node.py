import logging as root_logger

import acab.error.type_exceptions as te
from acab.modules.analysis.typing import util
from acab.modules.analysis.typing.util import TYPE_DEC_S
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

    def update(self, word, lookup=None):
        """ Post-addition update method.
        links self to a lookup-trie word if self is a variable """
        logging.debug("Node: {} updating with {}".format(self._value,
                                                         str(word)))
        # apply type if necessary
        if TYPE_DEC_S in word._data:
            self.unify_types(word._data[TYPE_DEC_S])

        if not (self.is_var == word.is_var):
            # complain if var status doesn't match
            raise te.TypeVariableConflictException(self)

        if self.is_var and self._var_node is None and lookup is not None:
            # if var, connect to var type trie
            self._var_node = lookup.add([self._value], [])
            self._var_node.add_node(self)

    def clear_var_node(self):
        self._var_node = None
