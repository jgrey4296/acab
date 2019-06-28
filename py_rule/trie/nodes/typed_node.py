from .trie_node import TrieNode

class M_TypedNode(TrieNode):

    def __init__(self, value):
        super().__init__(value)
        self._type = None

    def type_match_wrapper(self, node):
        if node._type is None:
            return
        self.type_match(node._type)

    def type_match(self, _type):
        if self._type is None:
            self._type = _type
            return self
        elif self._type != _type:
            raise te.TypeConflictException(self._type,
                                           _type,
                                           self.path)
        return None


