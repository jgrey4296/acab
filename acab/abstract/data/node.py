"""
AcabNode: The internal type which knowledge base data structures use.

"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

import logging as root_logger
from re import search
from uuid import uuid1
import weakref
from copy import deepcopy
from weakref import WeakValueDictionary, ref, proxy

from acab.config import AcabConfig

from acab.abstract.core.value import AcabValue

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

ROOT_S = util("Data.Struct", "ROOT_S")
BIND_S = util("Parsing.Structure", "BIND_S")

class AcabNode:
    """ The Base Node Class for Tries/Data structures etc"""

    @staticmethod
    def Root():
        """ Create a new root node """
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
        self._uuid_children = WeakValueDictionary()

        self._data = {}

        if data is not None:
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
        return self.has_child(v)

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


    def add_child(self, node):
        """ Add a node as a child of this node
        mutate object
        """
        assert(isinstance(node, AcabNode))
        self._children[node.name] = node
        self._uuid_children[node._uuid] = node
        return node

    def get_child(self, node):
        """ Get a node using a string, or a node itself """
        if isinstance(node, str):
            return self._children[node]

        return self._children[node.name]

    def has_child(self, node):
        """ Question if this node has a particular child """
        if isinstance(node, str):
            return node in self._children
        elif isinstance(node, AcabNode):
            return node._uuid in self._uuid_children
        elif isinstance(node, AcabValue):
            return node.name in self._children
        else:
            return False

    def remove_child(self, node):
        """ Delete a child from this node, return success state
        mutate object
        """
        result = None
        if self.has_child(node):
            result = self.get_child(node)
            if isinstance(node, str):
                del self._children[node]
            else:
                del self._children[node.name]


        return result

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
        raise DeprecationWarning("Use Print Semantics")

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


    def _default_setup(self, path: ['AcabNode'], data: Dict[Any,Any], context: Dict[Any,Any]):
        """ Called by a Semantics upon creation and addition of a new node,
        *if* an override hasn't been specified in the semnatic's value_constructor map"""
        pass
