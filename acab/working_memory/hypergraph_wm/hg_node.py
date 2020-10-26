from acab.abstract.data.node import AcabNode

class HGNode(AcabNode):
    """ A Node in a HyperGraph """

    def __init__(self, value):
        super().__init__(value)
        self._id    = None
        self._name  = None
        self._data  = {}
        self._edges = {}
