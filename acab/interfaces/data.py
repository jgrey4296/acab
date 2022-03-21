"""
A Collection of interfaces describing how data is used in the system.
Here, 'Data' means something analogous to ADTs
"""
# pylint: disable=multiple-statements,abstract-method,too-few-public-methods,invalid-sequence-index
from __future__ import annotations
import abc
import collections.abc as cABC
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator, Container,
                    Mapping, Match, MutableMapping, Protocol, Sequence, Tuple,
                    TypeAlias, TypeVar, cast, runtime_checkable)

from acab import types as AT

T = TypeVar('T', covariant=True, bound=AT.Node)

Value     : TypeAlias = "AT.Value[AT.ValueCore]"
Node      : TypeAlias = AT.Node
CtxIns    : TypeAlias = AT.CtxIns
Sen       : TypeAlias = AT.Sentence
Structure : TypeAlias = "AT.DataStructure[AT.TNode]"

@runtime_checkable
class _Node_p(cABC.Hashable, Protocol):

    @staticmethod
    @abc.abstractmethod
    def Root() -> Node:
        """ Construct a root node for a data structure """
        pass

    @abc.abstractmethod
    def _default_setup(self, *, path: list[Node], data: dict[Any,Any], context: dict[Any,Any]) -> None:
        """ Called by a Semantics upon creation of a new node """
        pass

    @abc.abstractmethod
    def _update_node(self, path:Sen, data:dict[str,Any], context:CtxIns) -> None:
        """ Called by a semantics for passing through a node """
        pass

    @abc.abstractmethod
    def add(self, node:Node) -> Node: pass

    @abc.abstractmethod
    def get(self, key:str) -> Node: pass

    @abc.abstractmethod
    def has(self, node:str|Node) -> bool: pass

    @abc.abstractmethod
    def remove(self, node:str|Node) -> None|Node: pass

    @abc.abstractmethod
    def clear(self) -> list[Node]: pass

    @abc.abstractmethod
    def key(self) -> str: pass

    @abc.abstractmethod
    def __iter__(self) -> Iterator[Node]: pass

    def __getitem__(self, key:str) -> Node:
        return self.get(key)

    def __delitem__(self, key:str|Node) -> None|Node:
        return self.remove(key)

# TODO factor 'root' out into AcabNodeStruct
@runtime_checkable
class _Structure_p(Container[T], Protocol):
    @staticmethod
    @abc.abstractmethod
    def build_default() -> Structure[T]: pass



@dataclass #type:ignore[misc]
class Node_i(_Node_p):
    """  """
    value    : Value
    children : dict[str, Node] = field(init=False, default_factory=dict)

@dataclass #type:ignore[misc]
class Structure_i(_Structure_p[T], Generic[T]):
    """ The structures which semantics operate on """
    root       : T              = field()
    components : dict[str, T]  = field(init=False, default_factory=dict)
