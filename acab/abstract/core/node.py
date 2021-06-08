"""
AcabNode: The internal type which knowledge base data structures use.

"""
import logging as root_logger
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from uuid import UUID, uuid1
from weakref import ref

import acab.abstract.interfaces.data_interfaces as DI
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue, Sentence

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
        return str(self.value)
    @property
    def var_set(self):
        return self.value.var_set

    def add_child(self, node) -> Node:
        """ Add a node as a child of this node
        mutate object
        """
        assert(isinstance(node, AcabNode))
        self.children[str(node)] = node
        return node

    def get_child(self, node) -> Node:
        """ Get a node using a string, or a node itself """
        if isinstance(node, str):
            return self.children[node]

        return self.children[str(node)]

    def has_child(self, term) -> bool:
        """ Question if this term has a particular child """
        # TODO handle if looking for a variable
        if isinstance(term, str):
            return term in self.children
        elif isinstance(term, AcabNode):
            return term.name in self.children
        # elif isinstance(term, AcabValue) and term.is_var:
        #     return term.name in self.children
        elif isinstance(term, AcabValue):
            return str(term) in self.children
        else:
            return False

    def remove_child(self, node) -> Optional[Node]:
        """ Delete a child from this node, return success state
        mutate object
        """
        result = None
        if self.has_child(node):
            result = self.get_child(node)
            if isinstance(node, str):
                del self.children[node]
            else:
                del self.children[str(node)]


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

    def parentage(self) -> List[Node]:
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


    def _default_setup(self, path: [Node], data: Dict[Any,Any], context: Dict[Any,Any]):
        """ Called by a Semantics upon creation of a new node """
        pass
    def _update_node(self, path, data, context):
        """ Called by a semantics for passing through a node """
        pass
