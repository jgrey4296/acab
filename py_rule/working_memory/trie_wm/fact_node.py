""" The Core Trie-Node, stores information, meta data """
import logging as root_logger

from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.node import PyRuleNode
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.abstract.printing import util as PrU

logging = root_logger.getLogger(__name__)
# see https://docs.python.org/3/library/weakref.html#module-weakref


class FactNode(PyRuleNode):
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

        assert(isinstance(node, PyRuleNode))
        temp_data = {}
        temp_data.update(node._data)
        if WMU.OPERATOR_S not in node._data:
            temp_data[WMU.OPERATOR_S] = WMU.EXOP.DOT
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

    def __contains__(self, other):
        if self.has_child(other):
            return self.get_child(other)._data[WMU.OPERATOR_S] == other._data[WMU.OPERATOR_S]
        return False

    def __hash__(self):
        return hash(str(self))

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
        assert(isinstance(fact, PyRuleNode))
        if fact in self:
            potential = self.get_child(fact)
            if fact._data[WMU.OPERATOR_S] == potential._data[WMU.OPERATOR_S]:
                return potential

        return None

    def delete_node(self, fact):
        """ Remove a Node from this Node """
        assert(isinstance(fact, PyRuleNode))
        if self.remove_child(fact):
            self._set_dirty_chain()

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
