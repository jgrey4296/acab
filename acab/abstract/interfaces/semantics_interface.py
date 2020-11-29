#!/usr/bin/env python3

#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import abc


class SemanticInterface(metaclass=abc.ABCMeta):
    """  """

    @abc.abstractmethod
    def down(self, value: Any) -> List['Sentence']:
        pass

    @abc.abstractmethod
    def up(self, sens: List['Sentence']) -> Any:
        pass



    @abc.abstractmethod
    def retrieve_semantics(self) -> 'AcabNodeSemantics':
        pass
    # TODO type this:
    @abc.abstractmethod
    def value_constructor(self) -> Any:
        pass


class PrintSemanticInterface(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def print(self, values: List['Printable']) -> str:
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
    def up(self, word, constructor) -> 'AcabNode':
        pass

    @abc.abstractmethod
    def down(self, node) -> 'AcabValue':
        pass

    @abc.abstractmethod
    def accessible(self, node: 'AcabNode',
                   data: Dict[Any, Any],
                   term: 'AcabValue') -> List['AcabNode']:
        """
        Retrieve a list of all nodes accessible from this node,
        according to a constraint term
        """
        pass

    @abc.abstractmethod
    def equal(self, word: 'AcabNode', word2: 'AcabNode') -> bool:
        pass

    @abc.abstractmethod
    def add(self, node: 'AcabNode', word: 'AcabValue', node_constructor: Callable) -> Tuple[bool, 'AcabNode']:
        pass

    @abc.abstractmethod
    def get(self, node: 'AcabNode', query_term: 'AcabValue') -> Optional['AcabNode']:
        """ Getting a node from the data structure """
        pass

    @abc.abstractmethod
    def contain(self, node: 'AcabNode', query_term: 'AcabValue') -> bool:
        """ Getting Node inclusion in a set """
        pass

    @abc.abstractmethod
    def delete(self, node: 'AcabNode', to_delete: 'AcabValue') -> Optional['AcabNode']:
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
    def add(self, structure : 'DataStructure', to_add : List['Sentence'], **kwargs) -> List['AcabNode']:

        pass
    @abc.abstractmethod
    def query(self, structure, clause : 'Sentence', ctxs : 'Contexts', engine : 'Engine') -> 'Contexts':
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
