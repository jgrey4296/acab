""" The Core Trie-Node, stores information, meta data """
from .trie_node import TrieNode
from math import floor
from py_rule.utils import EXOP, ROOT
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
        return FactNode(ROOT, EXOP.DOT)

    def is_root(self):
        """ Query to check if this node is the Root Node """
        return self._op is EXOP.ROOT


    @staticmethod
    def copy_fact(node):
        if isinstance(node, FactNode):
            return node.copy()
        else:
            assert(isinstance(node, TrieNode))
            operator = EXOP.DOT
            if 'op' in node._data:
                operator = node._data['op']
            new_node = FactNode(node._value,
                                operator,
                                data=node._data)
            return new_node

    def __init__(self,
                 value,
                 operator,
                 parent=None,
                 data=None):
        assert(isinstance(operator, EXOP))
        if parent is not None:
            parent = weakref.ref(parent)

        super().__init__(value, data)

        self._parent = parent
        self._dirty = True
        self._cached = []
        self._op = operator

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

    def __repr__(self):
        return "FactNode: {}".format(str(self))

    def __hash__(self):
        return hash(str(self))


    def copy(self):
        assert(not bool(self._children))
        #todo: deeper copy
        return FactNode(self._value, self._op)

    def set_parent(self, parent):
        assert(isinstance(parent, FactNode))
        self._parent = weakref.ref(parent)


    def insert(self, fact):
        """ Insert A Node as a Child of this Node """
        self._set_dirty_chain()
        copied = FactNode.copy_fact(fact)
        copied.set_parent(self)
        if copied._op is EXOP.EX and bool(self._children):
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
        assert(isinstance(fact, TrieNode))
        if fact in self:
            return self.get_child(fact)

        return None

    def delete_node(self, fact):
        """ Remove a Node from this Node """
        assert(isinstance(fact, TrieNode))
        if self.remove_child(fact):
            self._set_dirty_chain()


    def bind(self, data):
        """ Annotate the Node with a Meta-Bind Evaluation """
        if not self._data['bind']:
            return self.copy()
        else:
            copied = self.copy()
            copied._bind_to_value(data)
            return copied


    def search_regex(self, regex):
        """ Test a regex on the Nodes value """
        result = re.search(regex._value, self._value)
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

    def _bind_to_value(self, data):
        """ Set the Nodes value to be one retrieved from passed in bindings """
        assert(self._value in data)
        self._value = data[self._value]


