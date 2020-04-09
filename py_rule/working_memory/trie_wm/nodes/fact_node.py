""" The Core Trie-Node, stores information, meta data """
import logging as root_logger

from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.trie.nodes.trie_node import TrieNode
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.abstract.printing import util as PrU

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

        assert(isinstance(node, TrieNode))
        temp_data = {}
        temp_data.update(node._data)
        if WMU.OPERATOR_S not in node._data:
            temp_data._data[WMU.OPERATOR_S] = WMU.EXOP.DOT
        new_node = FactNode(node._value, data=temp_data)
        # TODO add original tags, vars and possible update type_str
        return new_node


    def __init__(self, value, data=None, type_str=None, tags=None, name=None):
        assert isinstance(data[WMU.OPERATOR_S], WMU.EXOP), data
        if data is None:
            data = WMU.DEFAULT_NODE_DATA.copy()

        super().__init__(value, data, type_str=type_str, tags=tags, name=name)

        self._dirty = True
        self._cached = []

    def __eq__(self, other):
        """ Main comparison routine: turn to strings, compare """
        assert(isinstance(other, FactNode))
        return str(self) == str(other)

    def __hash__(self):
        return hash(str(self))

    def __contains__(self, other):
        if self.has_child(other):
            return self.get_child(other)._data[WMU.OPERATOR_S] == other._data[WMU.OPERATOR_S]
        return False


    def copy(self):
        assert(not bool(self._children))
        val = self._value
        if isinstance(val, PyRuleValue):
            val = self._value.copy()

        copied = FactNode(val,
                          data=self._data,
                          type_str=self.type,
                          tags=self._tags,
                          name=self._name)
        return copied

    def insert(self, fact):
        """ Insert A Node as a Child of this Node """
        if fact in self:
            return self.get_child(fact)

        copied = FactNode.copy_fact(fact)
        copied.set_parent(self)

        # deal with exclusion
        if self._data[WMU.OPERATOR_S] == WMU.EXOP.EX:
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
        assert(util.BIND_S in self._data and self._data[util.BIND_S])
        self._value = data[self._value]
        self._data[util.BIND_S] = False
