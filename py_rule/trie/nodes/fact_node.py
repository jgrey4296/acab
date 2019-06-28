""" The Core Trie-Node, stores information, meta data """
from .trie_node import TrieNode
from math import floor
from py_rule.utils import EXOP
import IPython
import logging as root_logger
import py_rule.utils as util
import re
import weakref

logging = root_logger.getLogger(__name__)
#see https://docs.python.org/3/library/weakref.html#module-weakref

class FactNode(TrieNode):
    """ Both the type of a node in the trie,
    and the representation of data to add into the trie """

    @staticmethod
    def Root():
        """ Get a Root designated node """
        #TODO: make this a singleton?
        return FactNode("root", EXOP.ROOT)

    def is_root(self):
        """ Query to check if this node is the Root Node """
        return self._op is EXOP.ROOT


    def __init__(self, value, operator,
                 parent=None,
                 meta_leaf=None,
                 meta_eval=None):
        assert(isinstance(operator, EXOP))
        if parent is not None:
            parent = weakref.ref(parent)

        bind = False
        if isinstance(value, util.Bind):
            value = value.value
            bind = True

        super().__init__(value)

        self._parent = parent
        self._dirty = True
        self._cached = []
        self._op = operator

        #meta field holds ordering for leaf pathways
        #and eval details as a subtrie for other nodes
        self._is_meta = False #todo: implmement _is_meta
        self._meta_leaf = {}
        self._meta_eval = {}

        #after value setting to override based on passed in meta ops
        self.set_meta_eval(util.META_OP.BIND, bind)
        if meta_eval is not None:
            for k, v in meta_eval.items():
                self.set_meta_eval(k, v)
        if meta_leaf is not None:
            for k, v in meta_leaf.items():
                self.set_meta_leaf(k, v)


    def __eq__(self, other):
        """ Main comparison routine: turn to strings, compare """
        assert(isinstance(other, FactNode))
        return str(self) == str(other)

    def _eq__alt(self, other):
        """ DFS comparison routine """
        assert(isinstance(other, FactNode))
        if self._value != other._value:
            logging.warning("Values not the same")
            return False
        if len(self._children) != len(other._children):
            logging.warning("children length not the same")
            return False
        if not all([x in other._children for x in self._children]):
            logging.warning("keys not in other")
            return False
        comp = all([self._children[x] == other._children[x] for x in self._children])
        return comp

    def _print_node(self):
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
            val = f.replace(".", "d")
            #value stringify
        elif isinstance(self._value, int):
            val = str(self._value)
        elif ' ' in self._value:
            val = '"' + str(self._value) + '"'
        else:
            val = str(self._value)
        #Then convert any meta comparisons:
        if util.META_OP.COMP in self._meta_eval \
           and bool(self._meta_eval[util.META_OP.COMP]):
            meta = "(" \
                   + ", ".join([repr(x) for x in self._meta_eval[util.META_OP.COMP]]) \
                   + ")"
        elif util.META_OP.RULEBIND in self._meta_eval:
            meta = "(^{})".format(self._meta_eval[util.META_OP.RULEBIND])
        else:
            meta = ""

        final_val = "{}{}{}{}".format(op, bind, val, meta)
        return final_val

    def _print_sub_trie(self):
        """ Create a multi line string of the entire sub trie """
        #A list of each self->leaf path
        leaf_list = self._reconstruct()

        if not bool(leaf_list):
            return self._print_node()
        else:
            finals = []
            #for each path, convert to a repr of it
            for l in leaf_list:
                finals.append("".join([x._print_node() for x in l]))

            return ",\n".join(finals)

    def __repr__(self):
        return "FactNode: {}".format(self._print_node())


    def root_str(self):
        """ Return a set of EL strings, treating this Node as the Root """
        xs = [str(y) for x, y in sorted(self._children.items())]
        return ",\n".join(xs)

    def copy(self):
        assert(not bool(self._children))
        #todo: deeper copy
        meta_evals = self._meta_eval.copy()
        meta_leaf = self._meta_leaf.copy()
        return FactNode(self._value, self._op,
                    meta_leaf=meta_leaf,
                    meta_eval=meta_evals)

    def set_parent(self, parent):
        assert(isinstance(parent, FactNode))
        self._parent = weakref.ref(parent)


    def is_exclusive(self):
        """ Checks for the exclusion operator in this node """
        return self._op is util.EXOP.EX

    def looks_exclusive(self):
        """ Checks for implicit exclusivity by having 0 or 1 children """
        return len(self) <= 1


    def set_meta_leaf(self, mType, value):
        """ Annotate this Node with a Meta-Leaf.
        (A Meta-Leaf is a Meta-Node that sets its container to be a leaf)
        """
        logging.info("Setting Meta Leaf: {}, {}, {}".format(repr(self),
                                                            str(mType),
                                                            repr(value)))
        assert(isinstance(mType, util.META_OP))
        self._meta_leaf[mType] = value

    def set_meta_eval(self, mType, values):
        """
        Set the Node to hold a Meta-Node which can be evaluated.
        """
        #todo
        assert(isinstance(mType, util.META_OP))
        if isinstance(values, list):
            self._meta_eval[mType] = [x.copy() for x in values]
        else:
            self._meta_eval[mType] = values

    def get_meta_eval(self, mType):
        """ Get any meta-eval nodes from this Node """
        if mType in self._meta_eval:
            return self._meta_eval[mType]
        else:
            return []


    def insert(self, fact):
        """ Insert A Node as a Child of this Node """
        assert(isinstance(fact, FactNode))
        self._set_dirty_chain()
        copied = fact.copy()
        copied.set_parent(self)
        if copied._op is EXOP.EX \
           and bool(self._children):
            if copied in self:
                temp = self.get_child(copied)
            else:
                temp = None
            self.clear_children()
            if temp is not None:
                self.add_child(temp)

        if not copied in self:
            self.add_child(copied)
            return copied
        else:
            return self.get_child(copied)

    def get(self, fact):
        """ Retrieve a Node from this Node """
        assert(isinstance(fact, FactNode))
        if fact in self:
            return self.get_child(fact)

        return None

    def delete_node(self, fact):
        """ Remove a Node from this Node """
        assert(isinstance(fact, FactNode))
        if self.remove_child(fact):
            self._set_dirty_chain()

    def bind(self, data):
        """ Annotate the Node with a Meta-Bind Evaluation """
        if self.get_meta_eval(util.META_OP.BIND) is False:
            return self.copy()
        else:
            copied = self.copy()
            copied._bind_to_value(data)
            return copied


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
        """ Test a regex on the Nodes value """
        result = re.search(regex.value, self._value)
        if result is not None:
            return result.groupdict()
        else:
            return None

    def test_regexs_for_matching(self, regexs, currentData, preupdate=None):
        """ Test a number of regexs on this Node """
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
                for k, v in result.items():
                    if k not in newData:
                        newData[k] = v
                    elif newData[k] != v:
                        invalidated = True
                        break

        if invalidated:
            return (None, None)
        else:
            return (newData, self)


    def _set_dirty_chain(self):
        """ Mark this Node as modified, up to the root """
        self._dirty = True
        if self._parent is not None:
            self._parent()._set_dirty_chain()

    def _unify(self, other):
        """ Test two tries to see if they can match with substitutions """
        #TODO
        # { bindNode : [ options ] }
        raise Exception("Unimplemented")

    def _reconstruct(self):
        """ Internal method for DFS reconstructing min fact/leaf list """
        if not self._dirty and bool(self._cached):
            return self._cached
        #todo: improve caching
        queue = [([], self)]
        leaves = []
        while bool(queue):
            path, node = queue.pop()
            newpath = path.copy()
            newpath.append(node)
            if bool(node._children):
                queue += [(newpath, x) for x in node._children.values()]
            else:
                leaves.append(newpath)

        self._dirty = False
        self._cached = leaves
        return leaves

    def _bind_to_value(self, data):
        """ Set the Nodes value to be one retrieved from passed in bindings """
        assert(self._value in data)
        self._value = data[self._value]


#todo: add breadth and depth traversal
