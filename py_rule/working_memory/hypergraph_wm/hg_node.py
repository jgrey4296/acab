from py_rule.abstract.node import PyRuleNode

class HGNode(PyRuleNode):
    """ A Node in a HyperGraph """

    def __init__(self):
        super().__init__()
        self._id    = None
        self._name  = None
        self._data  = {}
        self._edges = {}
