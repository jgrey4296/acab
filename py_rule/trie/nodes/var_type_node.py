from .typed_trie_node

class VarTypeTrieNode(M_TypedNode):

    def __init__(self, value, _type=None):
        super().__init__(value)
        self._type = _type
        self.nodes = set([])
        self.var_names = set([])

    def __repr__(self):
        if self._type is not None:
            return "VarType: {} ({})".format(repr(self._type), len(self.nodes))
        else:
            return "VT: 0 {}".format(len(self.nodes))

    def add_var_name(self, node):
        if node.is_var():
            self.var_names.add(node.name)

    def add_node(self, node):
        self.nodes.add(node)
        node.is_var = True
        node.var_node = self

    def propagate(self):
        if self._type is not None:
            for n in self.nodes:
                n.type_match(self._type)

    def merge(self, nodes):
        assert(all([isinstance(x, VarTypeTrieNode) for x in nodes]))
        logging.debug("Merging Variables: {} into {}".format(", ".join([x.name for x in nodes]), self.name))
        #Get the set of all types for the variables
        all_types = {x._type for x in nodes if x._type is not None}
        # update self to point to all assignment nodes
        [self.add_node(y) for x in nodes for y in x.nodes]
        [self.type_match(x) for x in all_types]
        self.propagate()


