#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass,  field

import abc

Node = 'NodeInterface'
Value = 'AcabValue'

@dataclass
class NodeInterface(metaclass=abc.ABCMeta):
    """  """

    value : Value
    children : Dict[str, Node] = field(init=False, default_factory=dict)

    @staticmethod
    @abc.abstractmethod
    def Root() -> Node:
        """ Construct a root node for a data structure """
        pass


    @abc.abstractmethod
    def _default_setup(self, path: [Node], data: Dict[Any,Any], context: Dict[Any,Any]):
        """ Called by a Semantics upon creation of a new node """
        pass

    @abc.abstractmethod
    def _update_node(self, path, data, context):
        """ Called by a semantics for passing through a node """
        pass






    @abc.abstractmethod
    def add_child(self, node) -> Node:
        pass
    @abc.abstractmethod
    def get_child(self, node) -> Node:
        pass
    @abc.abstractmethod
    def has_child(self, node) -> bool:
        pass
    @abc.abstractmethod
    def remove_child(self, node) -> Node:
        pass
    @abc.abstractmethod
    def clear_children(self):
        pass


@dataclass
class StructureInterface(metaclass=abc.ABCMeta):
      """  """
      root      : 'AcabNode'               = field(init=False)
      semantics : 'AcabStructureSemantics' = field(default=None)

      @staticmethod
      @abc.abstractmethod
      def build_default() -> Any:
          pass
