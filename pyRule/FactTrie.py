from .FactNode import FactNode, EXOP
from .FactParse import parseString
from .Contexts import Contexts

class FactTrie:

    def __init__(self):
        self._root = FactNode.Root()
        self._last_node = self._root

    def assertS(self,s):
        self.assertFact(parseString(s))

    def retractS(self,s):
        self.retractFact(parseString(s))
        
    def assertFact(self, factList):
        assert(all([isinstance(x, FactNode) for x in factList]))
        assert(factList[0].is_root())
        self._clear_last_node()
        for newNode in factList[1:]:
            self._last_node = self._last_node.insert(newNode)


    def retractFact(self, factList):
        assert(all([isinstance(x, FactNode) for x in factList]))
        assert(factList[0].is_root())
        #go down to the child, and remove it
        self._clear_last_node()
        lastInList = factList.pop()
        
        for node in factList[1:-1]:
            self._last_node = self._last_node.get(node)
            if self._last_node is None:
                return

        self._last_node.delete_node(lastInList)


    def _clear_last_node(self):
        self._last_node = self._root

    def queryFact(self, factList):
        self._clear_last_node()
        initial_context = Contexts.initial()
        self._internal_query(factList, initial_context)
        return

    def _internal_query(self, factList, ctxs):
        #Go down the trie, running each test as necessary
        #annotating contexts as necessary
        self._root
        return

    

    
