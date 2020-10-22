from uuid import uuid1



class DataStructure:
    """
    The Abstract DataStructure Class
    Anything that wants to use StructureSemantics has to fulfill this
    Like AcabNode, is not a value, thus not directly able to be talked about
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
