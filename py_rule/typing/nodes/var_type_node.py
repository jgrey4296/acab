from .typed_node import M_TypedNode
from py_rule.typing.util import is_var

class VarTypeTrieNode(M_TypedNode):

    def __init__(self, value, _type=None):
        super().__init__(value)
        self._type = _type
        self._nodes = set([])
        self._var_names = set([])

    def __repr__(self):
        if self._type is not None:
            return "VarType: {} ({})".format(repr(self._type), len(self.nodes))
        else:
            return "VT: 0 {}".format(len(self.nodes))

    def add_var_name(self, node):
        self._var_names.add(node._value)

    def add_node(self, node):
        assert(is_var(node._value))
        self._nodes.add(node)
        self.add_var_name(node._value)
        self.type_match_wrapper(node._value)
        node._is_var = True
        node._var_node = self

    def propagate(self):
        if self._type is not None:
            for n in self._nodes:
                n.type_match(self._type)

    def merge(self, nodes):
        assert(all([isinstance(x, VarTypeTrieNode) for x in nodes]))
        logging.debug("Merging Variables: {} into {}".format(", ".join([x._value for x in nodes]), self._value))
        # update self to point to all assignment nodes
        [self.add_node(y) for x in nodes for y in x._nodes]

