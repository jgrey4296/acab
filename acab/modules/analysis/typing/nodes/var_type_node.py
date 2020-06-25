import logging as root_logger

from .typed_node import MonoTypedNode

logging = root_logger.getLogger(__name__)


class VarTypeTrieNode(MonoTypedNode):
    """ Node describing a variable's type """

    def __init__(self, value, _type=None):
        super().__init__(value, _type=_type)
        self._nodes = set([])
        self._var_names = set([])


    def add_node(self, node):
        assert(isinstance(node, MonoTypedNode))
        self._nodes.add(node)
        if node.is_var:
            self._var_names.add(node.name)
        # TODO: make this a weak ref?
        node._var_node = self

    def propagate(self):
        if self.type_instance is not None:
            for n in self._nodes:
                n.unify_types(self.type_instance)

    def merge(self, nodes):
        assert(all([isinstance(x, VarTypeTrieNode) for x in nodes]))
        logging.debug("Merging Variables: {} into {}".format(", ".join([str(x.name) for x in nodes]),
                                                             self.value))
        # update self to point to all assignment nodes
        var_nodes = [y for x in nodes for y in x._nodes]
        for n in var_nodes:
            self.add_node(n)

    def clear_assignments(self):
        for node in self._nodes:
            node.clear_var_node()
        self._nodes = set([])
        self._type_instance = None
