"""
Semantics:
Subdivides into *Complete Systems*, *Incomplete Mixins* and *Handlers*

All semantic systems should be able to lift basic sentences up to their preferred internal data format.
And reduce those internal formats back down to basic sentences.

SemanticMap also provide the ability to map a value or node to particular semantics,
and specifies *how* to search for the correct mapping.

Meanwhile IndependentSemantics_i are concerned only with the values and structures they have control over.

*Dependent* Semantics factor in contexts and a reference to the engine.


"""

import abc
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.abstract.interfaces.util as SU
from acab.abstract.config.config import AcabConfig, ConfigSpec
from acab.abstract.interfaces.handler_system import (
    HandlerComponent_i, HandlerSystem_i)
from acab.abstract.interfaces.value import (Sentence_i,
                                                       Value_i)
from acab.error.acab_print_exception import AcabPrintException
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.semantics.context_container import ContextContainer

from acab.abstract.core.default_structure import QUERY

Node               = 'AcabNode'
Sentence           = 'Sentence'
Printable          = 'Printable'
Value              = 'AcabValue'
Structure          = 'AcabStruct'
Engine             = 'Engine'
CtxCon             = 'ContextContainer'
Handler            = 'SemanticHandler' # Callable
AbsDepSemantics    = Union['AbstractionSemantics_i', 'DependentSemantics_i']
InDepSemantics     = 'IndependentSemantics_i'
ProductionOperator = 'ProductionOperator'
ModuleComponents   = 'ModuleComponents'

# Note: for dependent and indep, you retrieve semantics of a node,
# for *abstractions*, you're getting the semantics of a *sentence*
#--------------------------------------------------
@dataclass
class Semantic_Fragment(metaclass=abc.ABCMeta):
    """ Dataclass of Semantic Handlers to be added to the system,
and any structs they require """
    dependent   : List[HandlerComponent_i] = field(default_factory=list)
    independent : List[HandlerComponent_i] = field(default_factory=list)
    abstraction : List[HandlerComponent_i] = field(default_factory=list)
    structs     : List[Structure]          = field(default_factory=list)

#----------------------------------------
@dataclass
class SemanticSystem_i(HandlerSystem_i):
    """
    Map Instructions to Abstraction/Dependent Semantics
    """
    # TODO possibly re-add hooks / failure handling
    @abc.abstractmethod
    def __call__(self, *instructions, ctxs=None, data=None) -> CtxCon:
        pass

    @abc.abstractmethod
    def to_sentences(self) -> List[Sentence]:
        # TODO run the dep_sem.to_sentences for each struct with a matching registration
        pass

    def extend(self, mods:List[ModuleComponents]):
        semantics = [y for x in mods for y in x.semantics]
        for sem in semantics:
            [self._register_handler(x) for x in sem.dependent]
            [self._register_handler(x) for x in sem.independent]
            [self._register_handler(x) for x in sem.abstraction]
            [self._register_struct(x) for x in sem.structs]

@dataclass
class DependentSemantics_i(SemanticSystem_i, HandlerComponent_i):
    """
    Dependent Semantics rely on the context they are called in to function
    and are built with specific mappings to independent semantics
    """

    def __call__(self, struct, sen, ctxs=None, data=None):
        if QUERY in sen.data and bool(sen.data[QUERY]):
            return self.query(struct, sen, ctxs=ctxs, data=data)

        return self.insert(struct, sen, ctxs=ctxs, data=data)

    def to_sentences(self, struct, data=None, ctxs=None):
        """ Reduce a struct down to sentences, for printing """
        raise NotImplementedError()

    def verify(self, instruction, data=None, ctxs=None):
        raise NotImplementedError()
    @abc.abstractmethod
    def insert(self, struct, sen, data):
        pass

    @abc.abstractmethod
    def query(self, struct, sen, data):
        pass

    @abc.abstractmethod
    def compatible(self, struct: Structure) -> bool:
        """ Called to check the semantics can handle the suggested struct """
        pass


class IndependentSemantics_i(HandlerComponent_i):
    """
    Independent Semantics which operate on values and nodes, without
    requiring access to larger context, or engine access
    """
    @abc.abstractmethod
    def make(self, val: Value, data:Dict[Any,Any]=None) -> Node:
        """ Take a value, and return a node, which has been up'd """
        pass
    @abc.abstractmethod
    def up(self, node: Node, data=None) -> Node:
        """ Take ANY node, and add what is needed
        to use for this semantics """
        pass

    def down(self, node: Node, data=None) -> Value:
        return node.value

    @abc.abstractmethod
    def access(self, node: Node, term: Value, data:Dict[Any,Any]=None) -> List[Node]:
        """ Can node A reach the given term """
        pass

    @abc.abstractmethod
    def insert(self, node: Node, new_node: Node, data:Dict[Any,Any]=None) -> Node:
        pass

    @abc.abstractmethod
    def remove(self, node: Node, term: Value, data:Dict[Any,Any]=None) -> Optional[Node]:
        pass


class AbstractionSemantics_i(HandlerComponent_i):
    """
    Semantics of Higher level abstractions
    eg: Rules, Layers, Pipelines...

    AbsSems use the total semantic system to call other AbSems, or
    DepSems
    """

    def verify(self, instruction):
        pass
    @abc.abstractmethod
    def __call__(self, instruction, semSys, ctxs=None, data=None):
        pass


#--------------------------------------------------
