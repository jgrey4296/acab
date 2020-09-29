from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue
from acab.abstract.data.node import AcabNode
from acab.abstract.data.contexts import Contexts

class DataStructure:
    """
    The Abstract DataStructure Class
    Anything that wants to use StructureSemantics has to fulfill this

    """
    def __init__(self, semantics=None, node_type=None):
        assert(semantics is not None)
        if node_type is not None:
            semantics.set_node_type(node_type)

        self._root = None
        self._semantics = semantics


    @property
    def root(self):
        return self._root
