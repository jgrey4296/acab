#!/usr/bin/env python3

#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import abc
from dataclasses import dataclass, field

Node          = 'AcabNode'
Sentence      = 'Sentence'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'DataStructure'
Engine        = 'Engine'
Contexts      = 'Contexts'
SemanticUnion = Union['IndependentSemantics', 'DependentSemantics']

T = TypeVar('T')
T2 = TypeVar('T2')

@dataclass
class SemanticLifter(Generic[T], metaclass=abc.ABCMeta):
    """  """

    @abc.abstractmethod
    def down(self, value: T) -> List[Sentence]:
        pass

    @abc.abstractmethod
    def up(self, sens: List[Sentence]) -> T:
        pass



@dataclass
class SemanticsMap(Generic[T], metaclass=abc.ABCMeta):

    mapping: Dict[T, SemanticUnion] = field(default_factory=dict)
    lifting: Dict[Value, T]         = field(default_factory=dict)

    def retrieve_semantics(self, T) -> SemanticUnion:
        """
        Map node -> its semantics
        """
        assert(isinstance(node, type))
        # TODO should I be using my type instances for semantics?
        curr = node
        retrieved = None
        descendents_to_update = []
        while retrieved is None and curr not in (object, None):
            if curr in self.node_semantics:
                retrieved = self.node_semantics[curr]
            else:
                curr = curr.__base__
                descendents_to_update.append(curr)

        if retrieved is None:
            raise AcabSemanticException("Missing Node Semantic Binding for: {}".format(node), None)

        if len(descendents_to_update) > 1:
            self.node_semantics.update({x : retrieved for x in descendents_to_update})

        return retrieved






    # TODO type this:
    def _retrieve_semantics(self, current_val: T) -> SemanticUnion:
        """
        use the_type (ie: python type) first, if its necessary, distinguish using type_instance

        Always returns, even if its just lambda x: str(x)
        """
        chosen: Callable = self._bottom_semantic
        # search_order: List[Callable[[AcabPrintSemantics, Printable], Optional[SemanticSpec]]] = []
        search_order = self._search_order[:]

        for x in search_order:
            search_f = x
            if x in self._search_lookup:
                search_f = self._search_lookup[x]

            result = search_f(self, current_val)
            if result is not None:
                chosen = result
                break

        # TODO update _type_semantics chain with found bindings from hierarchy

        return chosen

    @abc.abstractmethod
    def value_constructor(self) -> T:
        pass






class IndependentSemantics(Generic[T], metaclass=abc.ABCMeta):
    """ """
    @abc.abstractmethod
    def accessible(self, node: T,
                   data: Dict[Any, Any],
                   term: Value) -> List[T]:
        """
        Retrieve a list of all nodes accessible from this node,
        according to a constraint term
        """
        pass

    @abc.abstractmethod
    def equal(self, word: T, word2: T) -> bool:
        pass

    @abc.abstractmethod
    def add(self, node: T, word: List[Value]) -> Tuple[bool, T]:
        pass

    @abc.abstractmethod
    def get(self, node: T, query_term: Value) -> Optional[T]:
        """ Getting a node from the data structure """
        pass

    @abc.abstractmethod
    def contain(self, node: T, query_term: Value) -> bool:
        """ Getting Node inclusion in a set """
        pass

    @abc.abstractmethod
    def delete(self, node: T, to_delete: Value) -> Optional[T]:
        """ Removing a node from the data structure """
        pass


    @abc.abstractmethod
    def make(self, T):
        pass



#
#
# TODO this now becomes a *dependent* semantics
class DependentSemantics(Generic[T, T2], metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def test_candidates(self, term, candidate_triple, tests, engine: T2) -> List[Any]:
        pass

    @abc.abstractmethod
    def __call__(self, clause: Sentence, obj: T, ctxs: Contexts, engine: T2, override: Dict[Any, Any]) -> Contexts:
        pass






