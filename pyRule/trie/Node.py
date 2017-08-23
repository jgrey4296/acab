from pyRule.utils import EXOP
import logging as root_logger
logging = root_logger.getLogger(__name__)
from math import floor
import pyRule.utils as util
import weakref
import IPython
import re
#see https://docs.python.org/3/library/weakref.html#module-weakref

class Node:
    """ Both the type of a node in the trie,
    and the representation of data to add into the trie """ 

    def __init__(self, value, operator,
                 parent=None,
                 meta_leaf=None,
                 meta_eval=None):
        assert(isinstance(operator,EXOP))

        if parent is not None:
            self._parent = weakref.ref(parent)
        else:
            self._parent = None
        self._dirty = True
        self._cached = []
        #meta field holds ordering for leaf pathways
        #and eval details as a subtrie for other nodes
        self._is_meta = False
        self._meta_leaf = {}
        self._meta_eval = {}

        if meta_eval is not None:
            for k,v in meta_eval.items():
                self.set_meta_eval(k, v)
        if meta_leaf is not None:
            for k,v in meta_leaf.items():
                self.set_meta_leaf(k,v)

        if not isinstance(value, util.Bind):
            self._value = value
            self.set_meta_eval(util.META_OP.BIND, False)
        else:
            #If a bind, extract the value, and annotate the node
            self._value = value.value
            self.set_meta_eval(util.META_OP.BIND, True)
        self._op = operator
        self._children = {}
        
    def is_exclusive(self):
        return self._op is util.EXOP.EX

    def has_exclusive(self):
        return len(self) <= 1

    def __contains__(self, v):
        if isinstance(v, Node):
            return v._value in self._children
        else:
            return v in self._children
    
    def set_meta_leaf(self, mType, values):
        #todo
        assert(isinstance(mType, util.META_OP))
        assert(isinstance(values, list))
        self._meta_eval[mType] = [x.copy() for x in values]
        
    def set_meta_eval(self, mType, values):
        #todo
        assert(isinstance(mType, util.META_OP))
        if isinstance(values, list):
            self._meta_eval[mType] = [x.copy() for x in values]
        else:
            self._meta_eval[mType] = values

    def get_meta_eval(self, mType):
        if mType in self._meta_eval:
            return self._meta_eval[mType]
        else:
            return []
        
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
        op = util.EXOP_lookup[self._op]
        if self._meta_eval[util.META_OP.BIND]:
            bind = "$"
        else:
            bind = ""
            
        #reconverting floats
        if isinstance(self._value, float):
            f = str(self._value)
            val = f.replace(".","d")
            #value stringify
        elif isinstance(self._value, int):
            val = str(self._value)
        elif ' ' in self._value:
            val = '"' + str(self._value) + '"'
        else:
            val = str(self._value)
        #Then convert any meta comparisons:
        if util.META_OP.COMP in self._meta_eval \
           and len(self._meta_eval[util.META_OP.COMP]) > 0:
            meta = "(" \
                   + ", ".join([repr(x) for x in self._meta_eval[util.META_OP.COMP]]) \
                   + ")"
        else:
            meta = ""

        final_val = "{}{}{}{}".format(op,bind,val,meta)
        return final_val

    def __str__(self):
        """ Create a multi line string of the entire sub trie """
        #A list of each self->leaf path
        leaf_list = self._reconstruct()

        if len(leaf_list) == 0:
            return repr(self)
        else:
            finals = []
            #for each path, convert to a repr of it
            for l in leaf_list:
                finals.append("".join([repr(x) for x in l]))
                
            return ",\n".join(finals)

        
    def copy(self):
        assert(len(self._children) == 0)
        #todo: deeper copy
        meta_evals = self._meta_eval.copy()
        meta_leaf = self._meta_leaf.copy()
        return Node(self._value, self._op,
                    meta_leaf=meta_leaf,
                    meta_eval=meta_evals)

    def set_parent(self, parent):
        assert(isinstance(parent, Node))
        self._parent = weakref.ref(parent)
            
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
        copied.set_parent(self)
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
        if fact._value in self._children \
           and fact._op is self._children[fact._value]._op:
            return self._children[fact._value]
        else:
            return None

    def delete_node(self, fact):
        assert(isinstance(fact, Node))
        if fact._value in self._children \
           and fact._op is self._children[fact._value]._op:
            del self._children[fact._value]
            self._set_dirty_chain()

    def __len__(self):
        return len(self._children)


    def bind(self, data):
        if self.get_meta_eval(util.META_OP.BIND) is False:
            return self.copy()
        else:
            copied = self.copy()
            copied._bind_to_value(data)
            return copied


    def _bind_to_value(self, data):
        assert(self._value in data)
        self._value = data[self._value]
            

    def split_tests(self):
        """ Split tests into (alphas, betas, regexs) """
        comps = self.get_meta_eval(util.META_OP.COMP)
        assert(isinstance(comps, list))
        alphas = []
        betas = []
        regexs = []
        for c in comps:
            if c.is_regex_test():
                regexs.append(c)
            elif c.is_alpha_test():
                alphas.append(c)
            else:
                betas.append(c)
        return (alphas, betas, regexs)
        
    def search_regex(self, regex):
        result = re.search(regex.value, self._value)
        if result is not None:
            return result.groupdict()
        else:
            return None

    def test_regexs_for_matching(self, regexs, currentData, preupdate=None):
        newData = currentData.copy()
        if preupdate is not None:
            newData[preupdate[0]] = preupdate[1]
        invalidated = False
        for regex in regexs:
            if invalidated:
                break
            result = self.search_regex(regex)
            if result is None:
                invalidated = True
            else:
                for (k,v) in result.items():
                    if k not in newData:
                        newData[k] = v
                    elif newData[k] != v:
                        invalidated = True
                        break

        if invalidated:
            return (None, None)
        else:
            return (self, newData)

        
    #todo: add breadth and depth traversal
