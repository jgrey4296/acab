from enum import Enum

EXOP = Enum('EXOP', 'DOT EX ROOT')

class FactNode:
    """ Both the type of a node in the trie,
    and the representation of data to add into the trie """ 

    def __init__(self, value, operator):
        assert(isinstance(operator,EXOP))
        self._value = value
        self._op = operator
        self._children = {}

    def copy(self):
        return FactNode(self._value, self._op)
        
    def __str__(self):
        if self._op is EXOP.DOT:
            op = "."
        else:
            op = "!"
        return op + str(self._value)
            
    @staticmethod
    def Root():
        return FactNode("root", EXOP.ROOT)
        
    def is_root(self):
        return self._op is EXOP.ROOT
        
    def _clear_node(self):
        self._children = {}
        
    def insert(self, fact):
        assert(isinstance(fact,FactNode))
        copied = fact.copy()
        if copied._op is EXOP.EX \
           and copied._value not in self._children:
            self._clear_node()
            
        if copied._value not in self._children:
            self._children[copied._value] = copied
            return copied
        else:
            return self._children[copied._value]

    def get(self, fact):
        assert(isinstance(fact,FactNode))
        if fact._value in self._children and fact._op is self._children[fact._value]._op:
            return self._children[fact._value]
        else:
            return None

    def delete_node(self, fact):
        if fact._value in self._children and fact._op is self._children[fact._value]._op:
            del self._children[fact._value]

    def __len__(self):
        return len(self._children)
