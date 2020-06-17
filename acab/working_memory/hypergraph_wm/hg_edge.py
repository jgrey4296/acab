from .hg_node import HGNode


class HGEdge(HGNode):
    """ An Edge in a HyperGraph """

    def __init__(self):
        super().__init__()
        self._node_sets = {}
