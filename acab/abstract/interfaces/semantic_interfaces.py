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
Handler       = 'SemanticHandler'
SemanticUnion = Union['IndependentSemantics', 'DependentSemantics']

T  = TypeVar('T')
T2 = TypeVar('T2')

class SemanticHandler(metaclass=abc.ABCMeta):
    """ Limited Purpose object to provide python-defined functionality.
    Bound by a semantics *map* or used as the handler for a *listener*
    """

    def __call__(self):
        pass

class SemanticMixin(metaclass=abc.ABCMeta):
    """ A Collection of functionality which is meaningless when separated,
    but can not, on its own, sufficient to be a semantics """
    pass


#
class SemanticSystem(Generic[T], metaclass=abc.ABCMeta):
    """ A Complete semantic system """

    @abc.abstractmethod
    def up(self, sens: List[Sentence]) -> T:
        pass

    @abc.abstractmethod
    def down(self, value: T) -> List[Sentence]:
        pass

    @abc.abstractmethod
    def ask(self, sentence) -> Any:
        """ Is this sentence meaningful wrt self"""
        # Think Smalltalk
        pass

    @abc.abstractmethod
    def do(self, sentence) -> Any:
        pass




#
@dataclass
class SemanticsMap(SemanticSystem):
    """ Maps values/nodes to semantics,
    using a specified lookup ordering.
    handles contexts.

    expectations and guarantees are downstream
    """

    mapping          : Dict[T, SemanticUnion]                       = field(default_factory=dict)
    search_order     : List[Callable[[T], Optional[SemanticUnion]]] = field(default_factory=list)
    # Downward guarantees of what semantics may contextually rely upon
    guarantees      : List[Handler]          = field(default_factory=list)
    # Downward expectations of what semantics must publicly provide
    expectations    : List[SemanticUnion]    = field(init=False, default_factory=list)

    # The collected interface of all public facing semantics
    # used for getattr?
    _interface_union : Set[str]               = field(init=False, default_factory=set)
    # The collected interface of all contextual semantics
    # used for a ctx_getattr
    _contextual_union : Set[str]              = field(init=False, default_factory=set)

    def __post_init__(self):
        # assert a DEFAULT is in mapping

        # set a default search order if empty

        # interface union and contextual union

        # verify all mappings handle required interface
        for semantics in self.mapping.values():
            # TODO and check guarantees satisfy semantics' expectations
            # assert(not difference(semantics.expectations, self.guarantees))
            assert(all([isinstance(semantics, x) for x in self.expectations]))



    def setup_guarantees(self):
        # TODO specify when to setup these
        pass
    def get(self, T) -> SemanticUnion:
        """ Get a mapped semantics, using the search order """
        chosen: SemanticUnion = self.bottom_semantic
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
            self.mapping.update({x : retrieved for x in descendents_to_update})

        return chosen



    def run(self, T) -> Any:
        pass
#
class IndependentSemantics(SemanticSystem):
    """ Semantics which do *not* have any contextual requirements. """
    pass


@dataclass
class DependentSemantics(SemanticSystem):
    """ Semantics with contextual expectations """
    expectations: Set[Handler] = field(default_factory=set)

    pass


#
