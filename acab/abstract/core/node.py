"""
AcabNode: The internal type which knowledge base data structures use.

"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from re import Pattern, search
from uuid import uuid1, UUID
from weakref import ref, WeakValueDictionary, proxy
import logging as root_logger

from acab.abstract.config.config import AcabConfig

from acab.abstract.core.values import AcabValue, Sentence
import acab.abstract.interfaces.data_interfaces as DI

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

ROOT = AcabValue(name=config.value("Data", "ROOT"))

Node = 'AcabNode'

@dataclass
class AcabNode(DI.NodeInterface):
    """ The Base Node Class for Tries/Data structures etc
    Not an AcabValue, Uses the most basic semantics possible
    """
    # TODO make children a dict of dicts
    # value  : AcabValue       = field(default=None)
    data     : Dict[str, Any]  = field(default_factory=dict)
    path     : Sentence        = field(default=None)
    parent   : Node            = field(default=None)
    children : Dict[str, Node] = field(default_factory=dict)
    uuid     : UUID            = field(default_factory=uuid1)

    @staticmethod
    def Root():
        """ Create a new root node """
        return AcabNode(value=ROOT)


    def __post_init__(self):
        # Unwrap Nodes to avoid nesting
        # TODO should this be the case?
        if isinstance(self.value, AcabNode):
            raise TypeError("Nodes shouldn't have nodes inside them")
        assert(isinstance(self.value, AcabValue))



    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        return self.value.name

    def __repr__(self):
        return "{}({})".format(self.__class__.__name__,
                               str(self))

    def __len__(self):
        return len(self.children)

    def __bool__(self):
        return bool(self.children)

    def __contains__(self, v):
        return self.has_child(v)

    def __iter__(self):
        return iter(self.children.values())

    def __hash__(self):
        return hash(self.uuid)


    @property
    def name(self):
        return self.value.name
    @property
    def var_set(self):
        return self.value.var_set

    def add_child(self, node) -> 'AcabNode':
        """ Add a node as a child of this node
        mutate object
        """
        assert(isinstance(node, AcabNode))
        self.children[node.name] = node
        return node

    def get_child(self, node) -> 'AcabNode':
        """ Get a node using a string, or a node itself """
        if isinstance(node, str):
            return self.children[node]

        return self.children[node.name]

    def has_child(self, node) -> bool:
        """ Question if this node has a particular child """
        # TODO handle if looking for a variable
        if isinstance(node, str):
            return node in self.children
        elif isinstance(node, AcabNode):
            return node.name in self.children
        elif isinstance(node, AcabValue) and node.is_var:
            return node.name in self.children
        elif isinstance(node, AcabValue):
            return node.name in self.children
        else:
            return False

    def remove_child(self, node) -> Optional['AcabNode']:
        """ Delete a child from this node, return success state
        mutate object
        """
        result = None
        if self.has_child(node):
            result = self.get_child(node)
            if isinstance(node, str):
                del self.children[node]
            else:
                del self.children[node.name]


        return result

    def clear_children(self):
        """ Remove all children from this node
        mutate object
        """
        self.children = {}

    def set_parent(self, parent):
        """ Set the parent node to this node
        mutate object
        """
        assert(isinstance(parent, AcabNode))
        self.parent = ref(parent)

    def parentage(self) -> List['AcabNode']:
        """ Get the full path from the root to this node """
        path = []
        current = self
        while current is not None:
            path.insert(0, current)
            current = current.parent
        return path


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
        """ Called by a Semantics upon creation of a new node """
        pass
    def _update_node(self, path, data, context):
        """ Called by a semantics for passing through a node """
        pass
