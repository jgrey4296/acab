from uuid import uuid1

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue
from acab.abstract.data.node import AcabNode
from acab.abstract.data.contexts import Contexts


class DataStructure:
    """
    The Abstract DataStructure Class
    Anything that wants to use StructureSemantics has to fulfill this

    """
    def __init__(self, semantics):
        assert(semantics is not None)
        self._uuiid = uuid1()
        self._semantics = semantics
        self._root = semantics.make_root()

    @property
    def root(self):
        return self._root

    def __bool__(self):
        return bool(self._root)
