from pyRule.utils import EXOP
import logging as root_logger
logging = root_logger.getLogger(__name__)


class Node:
    """ Both the type of a node in the trie,
    and the representation of data to add into the trie """ 

    def __init__(self, value, operator):
        assert(isinstance(operator,EXOP))
        #todo: add parent
        self._value = value
        self._op = operator
        self._children = {}

    def root_str(self):
        xs = [str(y) for x,y in sorted(self._children.items())]
        return ",\n".join(xs)


    def __eq__(self, other):
        """ Main comparison routine: turn to strings, compare """
        assert(isinstance(other, Node))
        return str(self) == str(other)

    def _eq__alt(self,other):
        """ DFS comparison routine """
        assert(isinstance(other,Node))
        if self._value != other._value:
            logging.warning("Values not the same")
            return False
        if len(self._children) != len(other._children):
            logging.warning("children length not the same")
            return False
        if not all([x in other._children for x in self._children.keys()]):
            logging.warning("keys not in other")
            return False
        comp = all([self._children[x] == other._children[x] for x in self._children.keys()])
        return comp
        
    
        
    def __str__(self):
        #operator stringify
        if self._op is EXOP.DOT:
            op = "."
        else:
            op = "!"
        #value stringify
        if ' ' in self._value:
            val = op + '"' + str(self._value) + '"'
        else:
            val = op + str(self._value)
        #todo: if self._value is a number,
        #format according to the parser

        #children stringify
        xs = [val + str(y) for x,y in sorted(self._children.items())]

        if len(xs) == 0:
            return val
        else:
            return ",\n".join(xs)

        
    def copy(self):
        return Node(self._value, self._op)
        
            
    @staticmethod
    def Root():
        return Node("root", EXOP.ROOT)
        
    def is_root(self):
        return self._op is EXOP.ROOT
        
    def _clear_node(self):
        self._children = {}
        
    def insert(self, fact):
        assert(isinstance(fact,Node))
        copied = fact.copy()
        if copied._op is EXOP.EX \
           and len(self._children) > 1:
            temp = self._children[copied._value]
            self._clear_node()
            if temp is not None:
                self._children[copied._value] = temp
            
        if copied._value not in self._children:
            self._children[copied._value] = copied
            return copied
        else:
            return self._children[copied._value]

    def get(self, fact):
        assert(isinstance(fact,Node))
        if fact._value in self._children and fact._op is self._children[fact._value]._op:
            return self._children[fact._value]
        else:
            return None

    def delete_node(self, fact):
        if fact._value in self._children and fact._op is self._children[fact._value]._op:
            del self._children[fact._value]

    def __len__(self):
        return len(self._children)

    #todo: add breadth and depth traversal
