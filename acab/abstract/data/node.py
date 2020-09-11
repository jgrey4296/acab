"""
AcabNode: The internal type which knowledge base data structures use.

"""

from re import search
from uuid import uuid1
import weakref
from copy import deepcopy

from acab.abstract.printing import util as PrU
from acab.config import AcabConfig

from acab.abstract.core.value import AcabValue

util = AcabConfig.Get()

ROOT_S = util("Data.Trie", "ROOT_S")
BIND_S = util("Parsing.Structure", "BIND_S")

class AcabNode:
    """ The Base Node Class for Tries/Data structures etc"""

    @staticmethod
    def Root():
        """ Get a default defined root node """
        return AcabNode(ROOT_S)

    def __init__(self, value, data=None):
        #Unwrap Nodes to avoid nesting
        if isinstance(value, AcabNode):
            node = value
            value = deepcopy(node.value)

        # A Unique identifier for this node:
        self._uuid = uuid1()
        # Wrap in an AcabValue if necessary:
        self._value = AcabValue.safe_make(value)

        self._path = None
        self._parent = None
        self._children = {}

        self._data = {}

        if data is not None:
            logging.warning("Setting data of Node: {}".format(data))
            self._data.update(data)

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        uuid = str(self._uuid)
        uuid_chop = "{}..{}".format(uuid[:4],uuid[-4:])
        return "{}:{}".format(uuid_chop, self._value.name)

    def __repr__(self):
        return "{}({})".format(self.__class__.__name__,
                               str(self))

    def __len__(self):
        return len(self._children)

    def __bool__(self):
        return bool(self._children)

    def __contains__(self, v):
        raise DeprecationWarning("Deprecated to force ability to specify semantics. Use has_child")

    def __iter__(self):
        return iter(self._children.values())

    def __hash__(self):
        return hash(str(self))


    @property
    def value(self):
        return self._value

    @property
    def name(self):
        return self._value.name
    @property
    def var_set(self):
        return self.value.var_set

    @property
    def children(self):
        return self._children.values()


    def add_child(self, node, semantics=None):
        """ Add a node as a child of this node
        mutate object
        """
        assert(isinstance(node, AcabNode))
        if semantics is not None:
            return semantics.node_add(node)

        self._children[node.name] = node
        return node

    def get_child(self, node, semantics=None):
        """ Get a node using a string, or a node itself """
        if semantics is not None:
            return semantics.node_get(self, node)
        elif isinstance(node, str):
            return self._children[node]
        else:
            return self._children[node.name]

    def has_child(self, node, semantics=None):
        """ Question if this node has a particular child """
        if semantics is not None:
            return semantics.node_contain(self, node)
        elif isinstance(node, str):
            return node in self._children
        else:
            return node.name in self._children

    def remove_child(self, node, semantics=None):
        """ Delete a child from this node, return success state
        mutate object
        """
        if semantics is not None:
            return semantics.node_delete(self, node)
        elif node in self:
            if isinstance(node, str):
                del self._children[node]
            else:
                del self._children[node.name]
            return True

        return False

    def clear_children(self):
        """ Remove all children from this node
        mutate object
        """
        self._children = {}


    def set_parent(self, parent):
        """ Set the parent node to this node
        mutate object
        """
        assert(isinstance(parent, AcabNode))
        self._parent = weakref.ref(parent)

    def parentage(self):
        """ Get the full path from the root to this node """
        path = []
        current = self
        while current is not None:
            path.insert(0, current)
            current = current._parent
        return path


    def bind(self, bindings):
        """ Return a copy that has applied given bindings to its value
        TODO : deprecate
        """
        raise DeprecationWarning()

    def _bind_to_value(self, data):
        """ Set the Node's value to be one retrieved
        from passed in bindings """
        raise DeprecationWarning()

    def copy(self):
        raise DeprecationWarning()

    def pprint(self, opts=None, **kwargs):
        return self.value.pprint(opts, **kwargs)


    def set_data(self, data):
        raise DeprecationWarning()

    def unify(self, node):
        """
        TODO Unify a pair of tries together
        """
        raise NotImplementedError()

    def diff(self, node):
        """
        TODO compare two tries
        """
        raise NotImplementedError()
