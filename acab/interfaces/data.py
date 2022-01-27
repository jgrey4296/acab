"""
A Collection of interfaces describing how data is used in the system.
Here, 'Data' means something analogous to ADTs
"""
import abc
import collections.abc as cABC
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT

Value              = AT.Value
Node               = AT.Node
Structure          = AT.DataStructure

@dataclass
class Node_i(cABC.MutableMapping, cABC.Hashable):
    """  """

    value    : Value
    children : Dict[str, Node] = field(init=False, default_factory=dict)

    @staticmethod
    @abc.abstractmethod
    def Root() -> Node:
        """ Construct a root node for a data structure """
        pass


    @abc.abstractmethod
    def _default_setup(self, *, path: [Node], data: Dict[Any,Any], context: Dict[Any,Any]):
        """ Called by a Semantics upon creation of a new node """
        pass

    @abc.abstractmethod
    def _update_node(self, path, data, context):
        """ Called by a semantics for passing through a node """
        pass

    @abc.abstractmethod
    def add(self, node) -> Node:
        pass

    @abc.abstractmethod
    def get(self, node) -> Node:
        pass

    @abc.abstractmethod
    def has(self, node) -> bool:
        pass

    @abc.abstractmethod
    def remove(self, node) -> Node:
        pass

    @abc.abstractmethod
    def clear(self) -> List[Node]:
        pass


    @abc.abstractmethod
    def key(self) -> str:
        pass

    def __getitem__(self, key):
        return self.get(key)

    def __setitem__(self, key, value):
        raise NotImplementedError("Nodes don't directly set a key's value, use add")

    def __delitem__(self, key):
        return self.remove(key)

    def __iter__(self):
        return iter(self.children)

# TODO factor 'root' out into AcabNodeStruct
@dataclass
class Structure_i(cABC.MutableMapping):
    """ The structures which semantics operate on """
    root       : Node           = field()
    components : Dict[str, Any] = field(init=False, default_factory=dict)

    @staticmethod
    @abc.abstractmethod
    def build_default() -> Structure:
        pass

    @abc.abstractmethod
    def __bool__(self) -> bool:
        """ Is the Structure Empty? """
        pass
