import acab.abstract.interfaces.data_interfaces as DI

class HGNode(DI.NodeInterface):
    """ A Node in a HyperGraph """

    def __init__(self, value):
        super().__init__(value)
        self._id    = None
        self._name  = None
        self._data  = {}
        self._edges = {}
