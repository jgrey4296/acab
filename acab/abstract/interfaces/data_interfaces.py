#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass,  field

import abc

@dataclass
class NodeInterface(metaclass=abc.ABCMeta):
    """  """

    value : 'AcabValue'
    children : Dict[str, 'AcabNode'] = field(init=False, default_factory=dict)

    @staticmethod
    @abc.abstractmethod
    def Root() -> 'AcabNode':
        """ Construct a root node for a data structure """
        pass


    @abc.abstractmethod
    def _default_setup(self, path: ['AcabNode'], data: Dict[Any,Any], context: Dict[Any,Any]):
        """ Called by a Semantics upon creation of a new node """
        pass

    @abc.abstractmethod
    def _update_node(self, path, data, context):
        """ Called by a semantics for passing through a node """
        pass






@dataclass
class StructureInterface(metaclass=abc.ABCMeta):
      """  """
      root : 'AcabNode'                    = field(init=False)
      semantics : 'AcabStructureSemantics' = field(default=None)

      @abc.abstractmethod
      def add(self, sen) -> Any:
          pass

      @abc.abstractmethod
      def query(self, sen) -> Any:
          pass

      @abc.abstractmethod
      def contain(self, sen) -> Any:
          pass

      def trigger(self, *data) -> Any:
          """ Not abstract, as not all structures can be triggered """
          pass


      @staticmethod
      @abc.abstractmethod
      def build_default(self) -> Any:
          pass
