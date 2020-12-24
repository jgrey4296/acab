"""
Semantics:

All semantics should be able to lift basic sentences up to their preferred internal data format.
And reduce those internal formats back down to basic sentences.

SemanticMap also provide the ability to map a value or node to particular semantics,
and specifies *how* to search for the correct mapping.

Meanwhile IndependentSemantics are concerned only with the values and structures they have control over.

*Dependent* Semantics factor in contexts and a reference to the engine.


"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.abstract.semantics import util as SemUtil

import abc
from dataclasses import dataclass, field

Node          = 'AcabNode'
Sentence      = 'Sentence'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'AcabStruct'
Engine        = 'Engine'
Contexts      = 'Contexts'
SemanticUnion = Union['IndependentSemantics', 'DependentSemantics']

T = TypeVar('T')
T2 = TypeVar('T2')

class SemanticLifter(Generic[T], metaclass=abc.ABCMeta):
    """  """

    @abc.abstractmethod
    def up(self, sens: List[Sentence]) -> T:
        pass

    @abc.abstractmethod
    def down(self, value: T) -> List[Sentence]:
        pass





@dataclass
class SemanticsMap(Generic[T], metaclass=abc.ABCMeta):

    # TODO:
    _search_order: List[Callable]   = field(default_factory=list)
    mapping: Dict[T, SemanticUnion] = field(default_factory=dict)
    lifting: Dict[Value, T]         = field(default_factory=dict)
    bottom_semantic: Callable       = field(init=False, default=None)

    def retrieve_semantics(self, T) -> SemanticUnion:
        """
        use the_type (ie: python type) first, if its necessary, distinguish using type_instance

        Always returns, even if its just lambda x: str(x)
        """
        # TODO type this
        chosen: Callable = self.bottom_semantic
        descendents_to_update = []
        # search_order: List[Callable[[AcabPrintSemantics, Printable], Optional[SemanticSpec]]] = []
        search_order = self._search_order[:]

        for x in search_order:
            search_f = x
            if x in self._search_lookup:
                search_f = self._search_lookup[x]

            assert(callable(search_f))
            result = search_f(self, current_val)
            if result is not None:
                chosen = result
                break

        # TODO update _type_semantics chain with found bindings from hierarchy
        #
        if len(descendents_to_update) > 1:
            self.node_semantics.update({x : retrieved for x in descendents_to_update})

        return chosen

    @abc.abstractmethod
    def value_constructor(self, value: Value) -> T:
        pass






@dataclass
class ContextualSemantics(metaclass=abc.ABCMeta):
    context: List[SemUtil.ContextValue] = field(init=False, default_factory=list)
    stack: List[SemUtil.StackValue]     = field(init=False, default_factory=list)
    queue: List[SemUtil.SemBox]         = field(init=False, default_factory=list)
    accumulation: Dict[str, Any]        = field(init=False, default_factory=dict)

    def _add_to_context(self, value):
        if isinstance(value, str):
            self._context.append(value)
        elif isinstance(value, list):
            self._context += value
        else:
            raise Exception("Expected a str or a list")

    def _add_to_accumulation(self, value):
        assert isinstance(value, dict)
        self._accumulation.update(value)

    def _push_stack(self, data, sentinel, params):
        assert isinstance(data, list)
        self._stack.append((self._queue, self._context))

        if sentinel is not None:
            data.append((SemUtil.RET_enum.SENTINEL, data, sentinel, params))

        self._queue = data
        self._context = []

    def _pop_stack(self):
        if not bool(self._queue) and bool(self._stack):
            stack_q, stack_ctx = self._stack.pop()
            self._queue = stack_q
            self._context = stack_ctx

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
    def make(self, val: T):
        pass



class DependentSemantics(Generic[T, T2], metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def test_candidates(self, term, candidate_triple, tests, engine: T2) -> List[Any]:
        pass

    @abc.abstractmethod
    def __call__(self, clause: Sentence, obj: T, ctxs: Contexts, engine: T2, override: Dict[Any, Any]) -> Contexts:
        pass








