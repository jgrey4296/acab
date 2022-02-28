"""
A Collection of interfaces describing how data is used in the system.
Here, 'Data' means something analogous to ADTs
"""
import abc
import collections.abc as cABC
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Protocol,
                    Tuple, TypeAlias, TypeVar, cast)

from acab import types as AT

Value     : TypeAlias = AT.Value
Node      : TypeAlias = AT.Node
Structure : TypeAlias = AT.DataStructure

@dataclass
class _Node_d:
    """  """
    value    : Value
    children : dict[str, Node] = field(init=False, default_factory=dict)

class Node_i(cABC.Hashable, _Node_d):

    @staticmethod
    @abc.abstractmethod
    def Root() -> Node:
        """ Construct a root node for a data structure """
        pass

    @abc.abstractmethod
    def _default_setup(self, *, path: list[Node], data: dict[Any,Any], context: dict[Any,Any]):
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
    def get(self, key) -> None:
        pass

    @abc.abstractmethod
    def has(self, node) -> bool:
        pass

    @abc.abstractmethod
    def remove(self, node) -> Node:
        pass

    @abc.abstractmethod
    def clear(self) -> list[Node]:
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
class _Structure_d:
    """ The structures which semantics operate on """
    root       : Node           = field()
    components : dict[str, Any] = field(init=False, default_factory=dict)


class Structure_i(cABC.MutableMapping, _Structure_d):

    @staticmethod
    @abc.abstractmethod
    def build_default() -> Structure:
        pass

    @abc.abstractmethod
    def __bool__(self) -> bool:
        """ Is the Structure Empty? """
        pass
