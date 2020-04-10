import logging as root_logger

from py_rule.modules.analysis.typing.util import is_var

from .typed_node import MonoTypedNode

logging = root_logger.getLogger(__name__)


class VarTypeTrieNode(MonoTypedNode):
    """ Node describing a variable's type """

    def __init__(self, value, _type=None):
        super().__init__(value, _type=_type)
        self._nodes = set([])
        self._var_names = set([])


    def add_node(self, node):
        assert(is_var(node._value))
        self._nodes.add(node)
        self._var_names.add(node.name)
        self.type_match_wrapper(node._value)
        node._is_var = True
        node._var_node = self

    def propagate(self):
        if self.type_instance is not None:
            for n in self._nodes:
                n.type_match(self.type_instance)

    def merge(self, nodes):
        assert(all([isinstance(x, VarTypeTrieNode) for x in nodes]))
        logging.debug("Merging Variables: {} into {}".format(", ".join([str(x.name) for x in nodes]),
                                                             self._value))
        # update self to point to all assignment nodes
        dummy = [self.add_node(y) for x in nodes for y in x._nodes]

    def clear_assignments(self):
        for node in self._nodes:
            node.clear_var_node()
        self._nodes = set([])
        self._type_instance = None
