import logging as root_logger

from py_rule.modules.analysis.typing.util import is_var

from .typed_node import MonoTypedNode

logging = root_logger.getLogger(__name__)


class VarTypeTrieNode(MonoTypedNode):
    """ Node describing a variable's type """

    def __init__(self, value, _type=None):
        super().__init__(value)
        self._type = _type
        self._nodes = set([])
        self._var_names = set([])

    def __repr__(self):
        type_str = "0"
        if self._type is not None:
            type_str = repr(self._type)

        return "VarType( {} ; {})".format(type_str, len(self._nodes))

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
        logging.debug("Merging Variables: {} into {}".format(", ".join([str(x) for x in nodes]),
                                                             self._value))
        # update self to point to all assignment nodes
        dummy = [self.add_node(y) for x in nodes for y in x._nodes]

    def clear_assignments(self):
        for node in self._nodes:
            node.clear_var_node()
        self._nodes = set([])
        self._type = None
