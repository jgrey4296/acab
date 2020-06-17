""" The Core Trie-Node, stores information, meta data """
import logging as root_logger

from acab.abstract.value import AcabValue
from acab.abstract.node import AcabNode
from acab.working_memory.trie_wm import util as WMU
from acab.abstract.printing import util as PrU

logging = root_logger.getLogger(__name__)
# see https://docs.python.org/3/library/weakref.html#module-weakref


class FactNode(AcabNode):
    """ Both the type of a node in the trie,
    and the representation of data to add into the trie """

    @staticmethod
    def Root():
        """ Get a Root designated node """
        return FactNode(WMU.ROOT_S, WMU.DEFAULT_NODE_DATA)


    def __init__(self, value, data=None):
        default_data = WMU.DEFAULT_NODE_DATA.copy()
        if data is not None:
            default_data.update(data)
        super().__init__(value, default_data)

        self._dirty = True
        self._cached = []

    def __eq__(self, other):
        """ Main comparison routine: turn to strings, compare """
        assert(isinstance(other, FactNode))
        return str(self) == str(other)

    def __contains__(self, other):
        assert(isinstance(other, AcabValue))
        if self.has_child(other):
            if WMU.OPERATOR_S not in other._data:
                return True

            return self.get_child(other).exop == other._data[WMU.OPERATOR_S]
        return False

    def __hash__(self):
        return hash(str(self))


    @property
    def exop(self):
        if WMU.OPERATOR_S in self.value._data:
            return self.value._data[WMU.OPERATOR_S]
        else:
            return self._data[WMU.OPERATOR_S]

    def insert(self, fact):
        """ Insert A Node as a Child of this Node """
        assert(isinstance(fact, AcabValue))
        # deal with exclusion
        if fact in self:
            retrieved_node = self.get_child(fact)
            # deal with exclusion
            if self.exop == WMU.EXOP.EX:
                self.clear_children()
                self.add_child(retrieved_node)

            return retrieved_node

        new_node = FactNode(fact)
        new_node.set_parent(self)

        # deal with exclusion
        if self.exop == WMU.EXOP.EX:
            self.clear_children()

        self.add_child(new_node)
        return new_node

    def get(self, fact):
        """ Retrieve a Node from this Node """
        assert(isinstance(fact, AcabValue))
        potential = None
        operator = None
        if fact in self:
            potential = self.get_child(fact)
            operator = potential._data[WMU.OPERATOR_S]

        if fact._data[WMU.OPERATOR_S] != operator:
            potential = None

        return potential

    def delete_node(self, fact):
        """ Remove a Node from this Node """
        assert(isinstance(fact, AcabValue))
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
