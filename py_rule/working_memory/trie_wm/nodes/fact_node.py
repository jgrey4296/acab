""" The Core Trie-Node, stores information, meta data """
import logging as root_logger
import weakref

from py_rule.abstract.trie.nodes.trie_node import TrieNode
from py_rule.working_memory.trie_wm import util as WMU

logging = root_logger.getLogger(__name__)
# see https://docs.python.org/3/library/weakref.html#module-weakref


class FactNode(TrieNode):
    """ Both the type of a node in the trie,
    and the representation of data to add into the trie """

    @staticmethod
    def Root():
        """ Get a Root designated node """
        return FactNode(WMU.ROOT_S, WMU.DEFAULT_NODE_DATA)

    @staticmethod
    def copy_fact(node):
        """ Lift Trie-nodes to FactNodes as necessary """
        if isinstance(node, FactNode):
            return node.copy()
        else:
            assert(isinstance(node, TrieNode))
            operator = WMU.EXOP.DOT
            if WMU.OPERATOR_S in node._data:
                operator = node._data[WMU.OPERATOR_S]
            new_node = FactNode(node._value, operator, node._data)
            return new_node

    def __init__(self, value, data=None, parent=None):
        assert isinstance(data[WMU.OPERATOR_S], WMU.EXOP), data
        assert WMU.BIND_S in data, data
        assert WMU.VALUE_TYPE_S in data, data
        if parent is not None:
            parent = weakref.ref(parent)

        if data is None:
            data = WMU.DEFAULT_NODE_DATA.copy()

        super().__init__(value, data)

        self._op = data[WMU.OPERATOR_S]
        self._parent = parent
        self._dirty = True
        self._cached = []

    def __eq__(self, other):
        """ Main comparison routine: turn to strings, compare """
        assert(isinstance(other, FactNode))
        return str(self) == str(other)

    def __str__(self):
        val = super().__str__()

        if WMU.OPERATOR_S in self._data:
            val += WMU.EXOP_lookup[self._data[WMU.OPERATOR_S]]

        return val

    def __repr__(self):
        return "FactNode: {}".format(str(self))

    def __hash__(self):
        return hash(str(self))

    def __contains__(self, other):
        if self.has_child(other):
            return self.get_child(other)._data[WMU.OPERATOR_S] == other._data[WMU.OPERATOR_S]
        return False

    def copy(self):
        assert(not bool(self._children))
        # TODO: deeper copy
        return FactNode(self._value, data=self._data.copy())

    def set_parent(self, parent):
        assert(isinstance(parent, FactNode))
        self._parent = weakref.ref(parent)

    def parentage(self):
        path = []
        current = self
        while current is not None:
            path.insert(0, current)
            current = current._parent
        return path

    def insert(self, fact):
        """ Insert A Node as a Child of this Node """
        if fact in self:
            return self.get_child(fact)

        copied = FactNode.copy_fact(fact)
        copied.set_parent(self)

        # deal with exclusion
        if self._op is WMU.EXOP.EX:
            self.clear_children()

        self.add_child(copied)
        return copied

    def get(self, fact):
        """ Retrieve a Node from this Node """
        assert(isinstance(fact, TrieNode))
        if fact in self:
            potential = self.get_child(fact)
            if fact._data[WMU.OPERATOR_S] == potential._data[WMU.OPERATOR_S]:
                return potential

        return None

    def delete_node(self, fact):
        """ Remove a Node from this Node """
        assert(isinstance(fact, TrieNode))
        if self.remove_child(fact):
            self._set_dirty_chain()

    def bind(self, bindings):
        """ Annotate the Node with a Meta-Bind Evaluation """
        if not self._data[WMU.BIND_S]:
            return self.copy()
        else:
            copied = self.copy()
            copied._bind_to_value(bindings)
            return copied

    def _set_dirty_chain(self):
        """ Mark this Node as modified, up to the root """
        self._dirty = True
        if self._parent is not None:
            self._parent()._set_dirty_chain()

    def _unify(self, other):
        """ Test two tries to see if they can match with substitutions """
        # TODO
        # { bindNode : [ options ] }
        raise NotImplementedError()

    def _bind_to_value(self, data):
        """ Set the Nodes value to be one retrieved
        from passed in bindings """
        assert(self._value in data)
        self._value = data[self._value]
