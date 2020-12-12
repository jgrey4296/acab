#!/usr/bin/env python3

#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import abc

Node          = 'AcabNode'
Sentence      = 'Sentence'
NodeSemantics = 'AcabNodeSemantics'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'DataStructure'

class SemanticInterface(metaclass=abc.ABCMeta):
    """  """

    @abc.abstractmethod
    def down(self, value: Any) -> List[Sentence]:
        pass

    @abc.abstractmethod
    def up(self, sens: List[Sentence]) -> Any:
        pass



    @abc.abstractmethod
    def retrieve_semantics(self) -> NodeSemantics:
        pass
    # TODO type this:
    @abc.abstractmethod
    def value_constructor(self) -> Any:
        pass


class PrintSemanticInterface(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def print(self, values: List[Printable]) -> str:
        pass

    @abc.abstractmethod
    def ask(self, lookup, for_uuid=None) -> Any:
        pass
    @abc.abstractmethod
    def use(self, lookup, for_uuid=None):
        pass



class ProductionSemanticInterface(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def __call__(self, obj, ctxs, engine, override) -> Any:
        pass



class OperatorInterface(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def __call__(self, *params: List[Any], data: Dict[Any, Any], engine: 'Engine') -> Any:
        pass



class NodeSemantics(metaclass=abc.ABCMeta):
    """ """

    # TODO up and down
    @abc.abstractmethod
    def up(self, word: Node) -> Node:
        """ Lift a node"""
        pass

    @abc.abstractmethod
    def down(self, node) -> Value:
        pass

    @abc.abstractmethod
    def accessible(self, node: Node,
                   data: Dict[Any, Any],
                   term: Value) -> List[Node]:
        """
        Retrieve a list of all nodes accessible from this node,
        according to a constraint term
        """
        pass

    @abc.abstractmethod
    def equal(self, word: Node, word2: Node) -> bool:
        pass

    @abc.abstractmethod
    def add(self, node: Node, word: Value, node_constructor: Callable) -> Tuple[bool, Node]:
        pass

    @abc.abstractmethod
    def get(self, node: Node, query_term: Value) -> Optional[Node]:
        """ Getting a node from the data structure """
        pass

    @abc.abstractmethod
    def contain(self, node: Node, query_term: Value) -> bool:
        """ Getting Node inclusion in a set """
        pass

    @abc.abstractmethod
    def delete(self, node: Node, to_delete: Value) -> Optional[Node]:
        """ Removing a node from the data structure """
        pass


    @abc.abstractmethod
    def test_candidates(self, term, candidate_triple, tests, engine):
        pass




class StructureSemantics(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def init_struct(self, struct):
        pass


    @abc.abstractmethod
    def add(self, structure : Structure, to_add : List[Sentence], **kwargs) -> List[Node]:

        pass
    @abc.abstractmethod
    def query(self, structure, clause : Sentence, ctxs : 'Contexts', engine : 'Engine') -> 'Contexts':
        """ Answer a clause asked of the data structure """
        # TODO is this part of call semantics?
        # open / closed world
        # depth / breath search
        # match as pattern?
        # return type
        pass


    @abc.abstractmethod
    def contain(self, structure, sentence) -> bool:
        pass
