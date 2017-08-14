from pyRule.utils import EXOP
import logging as root_logger
logging = root_logger.getLogger(__name__)
from math import floor
import weakref
#see https://docs.python.org/3/library/weakref.html#module-weakref

class Node:
    """ Both the type of a node in the trie,
    and the representation of data to add into the trie """ 

    def __init__(self, value, operator, parent=None, meta_leaf=None, meta_eval=None):
        assert(isinstance(operator,EXOP))

        if parent is not None:
            self._parent = weakref.ref(parent)
        else:
            self._parent = None
        self._dirty = True
        self._cached = []
        
        self._value = value
        self._op = operator
        self._children = {}
        #meta field holds ordering for leaf pathways
        #and eval details as a subtrie for other nodes
        self._is_meta = False
        self._meta_leaf = {}
        self._meta_eval = {}

    def _set_dirty_chain(self):
        self._dirty = True
        if self._parent is not None:
            self._parent()._set_dirty_chain()
        
    def _unify(self, other):
        """ Test two tries to see if they can match with substitutions """
        # { bindNode : [ options ] }
        return {}

        
    def _reconstruct(self):
        """ Internal method for DFS reconstructing min fact/leaf list """
        if not self._dirty:
            return self._cached
        #todo: improve caching
        queue = [([], self)]
        leaves = []
        while len(queue) > 0:
            path, node = queue.pop()
            newpath = path.copy()
            newpath.append(node)
            if len(node._children) > 0:
                queue += [(newpath, x) for x in node._children.values()]
            else:
                leaves.append(newpath)

        self._dirty = False
        self._cached = leaves
        return leaves
                    
        
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

    def __repr__(self):
        """ Return a representation of this particular node """
        #operator stringify
        if self._op is EXOP.DOT:
            op = "."
        else:
            op = "!"
        #reconverting floats
        if isinstance(self._value, float):
            f = str(self._value)
            val = f.replace(".","d")
            #value stringify
        elif ' ' in self._value:
            val = '"' + str(self._value) + '"'
        else:
            val = str(self._value)

        final_val = op + val
        return final_val

    def __str__(self):
        """ Create a multi line string of the entire sub trie """
        leaf_list = self._reconstruct()

        if len(leaf_list) == 0:
            return repr(self)
        else:
            finals = []
            for l in leaf_list:
                finals.append("".join([repr(x) for x in l]))
                
            return ",\n".join(finals)

        
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
        self._set_dirty_chain()
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
            self._set_dirty_chain()

    def __len__(self):
        return len(self._children)

    #todo: add breadth and depth traversal
