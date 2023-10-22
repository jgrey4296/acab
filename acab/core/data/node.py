"""
AcabNode: The internal type which knowledge base data structures use.

"""
##-- imports
from __future__ import annotations
import logging as logmod
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ReferenceType, ref

import acab.interfaces.data as DI
import acab.interfaces.value as VI
import acab
from acab.core.util.decorators.util import cache
from acab.interfaces.value import ValueFactory as VF
from acab.core.util.debugging import logdel

##-- end imports
logging                = logmod.getLogger(__name__)

Node_A     : TypeAlias = DI.Node_i
Sentence_A : TypeAlias = VI.Sentence_i
Value_A    : TypeAlias = VI.Value_i

config                 = acab.config
ROOT                   = config.data.ROOT

@logdel
@dataclass
class AcabNode(DI.Node_i):
    """ The Base Node_A Class for Tries/Data structures etc
    Not an AcabValue, Uses the most basic semantics possible
    """
    # TODO make children a dict of dicts
    # value  : AcabValue       = field(default=None)
    data     : dict[str, Any]               = field(default_factory=dict)
    path     : None|Sentence_A              = field(default=None)
    parent   : None | ReferenceType[Node_A] = field(default=None)
    children : dict[str, Node_A]            = field(default_factory=dict)
    uuid     : UUID                         = field(default_factory=uuid1)

    @staticmethod
    def Root():
        """ Create a new root node """
        return AcabNode(value=VF.value(ROOT))

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
        return self.has(v)

    def __getitem__(self, v):
        if isinstance(v, VI.Sentence_i):
            current = self
            for x in v.words:
                current = current.get(x)
            return current
        else:
            return self.get(v)

    def __iter__(self):
        return iter(self.children.values())

    @cache
    def __hash__(self):
        return hash(self.uuid)

    @cache
    def key(self):
        """ Default Node_A->str key for child mapping """
        return self.value.key()

    def keys(self):
        return self.children.keys()

    @property
    def name(self):
        return str(self.value)

    def add(self, node:Node_A, *, key:str=None) -> Node_A:
        """ Add a node as a child of this node
        mutate object
        """
        assert(isinstance(node, DI.Node_i))
        if key is None:
            key = node.key()
        self.children[key] = node
        node.set_parent(self)
        return node

    def get(self, key:'str|Value_A|Node_A') -> Node_A:
        """ Get a node using a string, or a node itself """
        if isinstance(key, str) and key in self.children:
            return self.children[key]
        elif isinstance(key, str):
            matches = [x for x in self.keys() if key in x]
            return self.children[matches[0]]
        else:
            return self.children[key.key()]

    def has(self, key:'str|Value_A|Node_A|None'=None) -> bool:
        """ Question if this term has a particular child,
        by the simplest condition of whether there is a simple string
        mapping.
        """
        result = False
        is_str = isinstance(key, str)

        if key is None:
            result = bool(self)
        elif is_str:
            keys = self.keys()
            result = any([key in x for x in keys])
        elif isinstance(key, (DI.Node_i, VI.Value_i)):
            result = key.key() in self.children

        return result

    def remove(self, key:'str|Node_A') -> 'None|Node_A':
        """ Delete a child from this node, return success state
        mutate object
        """
        result = None
        if self.has(key):
            result = self.get(key)
            if isinstance(key, str):
                del self.children[key]
            else:
                del self.children[key.key()]

        return result

    def clear(self) -> list[Node_A]:
        """ Remove all children from this node
        mutate object
        """
        children = list(self.children.values())
        self.children = {}
        return children

    def set_parent(self, parent: Node_A):
        """ Set the parent node to this node
        mutate object
        """
        assert(isinstance(parent, DI.Node_i))
        self.parent = ref(parent)

    @property
    def parentage(self) -> Sentence_A:
        """ Get the full path from the root to this node """
        path = [self.value]
        current = self.parent
        while current is not None and current() is not None:
            deref : AcabNode = cast(AcabNode, current())
            path.append(deref.value)
            current = deref.parent

        path.reverse()
        return VF.sen(path) #type:ignore

    def _default_setup(self, path: list[Node_A], *, data: dict[Any,Any], context: dict[Any,Any]):
        """ Called by a Semantics upon creation of a new node """
        pass

    def _update_node(self, path, data, context):
        """ Called by a semantics for passing through a node """
        pass
