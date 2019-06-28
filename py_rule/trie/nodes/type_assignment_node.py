from .typed_trie_node import M_TypedNode

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
                                                 repr(self.name),
                                                 type_str)

    def update(self, node, lookup):
        logging.debug("Node: {} updating with {}".format(self.name,
                                                        node.name))
        self.type_match_wrapper(node)
        if not self.is_var and node.is_var():
            raise te.TypeVariableConflictException(self.path)

        if self.is_var and self.var_node is None:
            self.var_node = lookup.add([self.path[-1]], [])
            self.var_node.nodes.add(self)
            self.var_node.add_var_name(node)
            self.var_node.type_match_wrapper(node)
