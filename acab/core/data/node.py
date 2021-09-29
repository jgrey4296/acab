"""
AcabNode: The internal type which knowledge base data structures use.

"""
import logging as root_logger
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import UUID, uuid1
from weakref import ReferenceType, ref

from acab import types as AT
import acab.interfaces.data as DI
import acab.interfaces.value as VI
from acab.core.config.config import AcabConfig
from acab.core.data.values import AcabValue, Sentence

from acab.core.data.default_structure import ROOT
logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

Node  = AT.Node
Value = AT.Value

@dataclass
class AcabNode(DI.Node_i):
    """ The Base Node Class for Tries/Data structures etc
    Not an AcabValue, Uses the most basic semantics possible
    """
    # TODO make children a dict of dicts
    # value  : AcabValue       = field(default=None)
    data     : Dict[str, Any]                = field(default_factory=dict)
    path     : Sentence                      = field(default=None)
    parent   : Optional[ReferenceType]       = field(default=None)
    children : Dict[str, Node]               = field(default_factory=dict)
    uuid     : UUID                          = field(default_factory=uuid1)

    @staticmethod
    def Root():
        """ Create a new root node """
        return AcabNode(value=AcabValue.safe_make(ROOT))


    def __post_init__(self):
        if isinstance(self.value, DI.Node_i):
            raise TypeError("Nodes shouldn't have nodes inside them")
        if not isinstance(self.value, VI.Value_i):
            raise TypeError("Nodes Must have Values inside them")

    @cache
    def __str__(self):
        return self.value.name

    @cache
    def __repr__(self):
        return "{}({})".format(self.__class__.__name__,
                               str(self))

    def __len__(self):
        return len(self.children)

    def __bool__(self):
        return bool(self.children)

    def __contains__(self, v):
        return self.has_child(v)

    def __getitem__(self, v):
        if isinstance(v, Sentence):
            current = self
            for x in v.words:
                current = current.get_child(x)
            return current
        else:
            return self.get_child(v)

    def __iter__(self):
        return iter(self.children.values())

    @cache
    def __hash__(self):
        return hash(self.uuid)

    @cache
    def key(self):
        """ Default Node->str key for child mapping """
        return self.value.key()

    def keys(self):
        return self.children.keys()
    @property
    def name(self):
        return str(self.value)

    def add_child(self, node:Node, key:str=None) -> Node:
        """ Add a node as a child of this node
        mutate object
        """
        assert(isinstance(node, AcabNode))
        if key is None:
            key = node.key()
        self.children[key] = node
        node.set_parent(self)
        return node

    def get_child(self, key:Union[str, Value, Node]) -> Node:
        """ Get a node using a string, or a node itself """
        if isinstance(key, str) and key in self.children:
            return self.children[key]
        elif isinstance(key, str):
            matches = [x for x in self.keys() if key in x]
            return self.children[matches[0]]

        return self.children[key.key()]

    def has_child(self, key:Tuple[str, Value, None]) -> bool:
        """ Question if this term has a particular child,
        by the simplest condition of whether there is a simple string
        mapping.
        """
        if key is None:
            return bool(self)
        if isinstance(key, str):
            keys = self.keys()
            return any([key in x for x in keys])
        if isinstance(key, (AcabNode, AcabValue)):
            return key.key() in self.children

        return False

    def remove_child(self, key:Tuple[str, Node]) -> Optional[Node]:
        """ Delete a child from this node, return success state
        mutate object
        """
        result = None
        if self.has_child(key):
            result = self.get_child(key)
            if isinstance(key, str):
                del self.children[key]
            else:
                del self.children[key.key()]


        return result

    def clear_children(self) -> List[Node]:
        """ Remove all children from this node
        mutate object
        """
        children = list(self.children.values())
        self.children = {}
        return self.children

    def set_parent(self, parent: Node):
        """ Set the parent node to this node
        mutate object
        """
        assert(isinstance(parent, AcabNode))
        self.parent = ref(parent)

    @property
    def parentage(self) -> Sentence:
        """ Get the full path from the root to this node """
        path = [self.value]
        current = self.parent
        while current is not None and current() is not None:
            path.append(current().value)
            current = current().parent

        path.reverse()
        return Sentence.build(path)


    def _default_setup(self, path: [Node], data: Dict[Any,Any], context: Dict[Any,Any]):
        """ Called by a Semantics upon creation of a new node """
        pass
    def _update_node(self, path, data, context):
        """ Called by a semantics for passing through a node """
        pass
