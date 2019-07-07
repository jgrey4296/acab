from .typed_node import M_TypedNode
from py_rule.typing import util

class TypeAssignmentTrieNode(M_TypedNode):
    """ A Node in the Type Assignment Trie.
    Used in type inference.
    Enables linking with variable type trie """

    def __init__(self, value, _type=None, var_node=None):
        super().__init__(value)
        self._type = _type
        self._var_node = var_node
        self._is_var = var_node is not None

    def __repr__(self):
        type_str = ""
        var_str = ""
        if self._type is not None:
            type_str = repr(self._type)
        if self._is_var:
            var_str = "$"
        return "Type Assignment: {}{} {}".format(var_str,
                                                 repr(self._value),
                                                 type_str)

    def update(self, node, lookup):
        """ Post-addition update method.
        links self to a lookup-trie node if self is a variable """
        logging.debug("Node: {} updating with {}".format(self._value,
                                                         node._value))
        #apply type if necessary
        self.type_match_wrapper(node)
        if not self._is_var and util.is_var(node):
            #complain if var status doesn't match
            raise te.TypeVariableConflictException(self._path)

        if self._is_var and self._var_node is None:
            #if var, connect to var type trie
            self._var_node = lookup.add([self._path[-1]], [])
            self._var_node.add_node(self)
