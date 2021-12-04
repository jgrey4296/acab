"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators

Structure is:
ProductionOperator  : The implementation
ProductionComponent : Pairs the Operator with bindings
ProductionContainer : Groups Components together

"""
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace, FrozenInstanceError
from fractions import Fraction
from re import Pattern
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import UUID, uuid1
from weakref import ref

from acab import types as AT
import acab.core.data.default_structure as DS
from acab.core.config.config import AcabConfig
from acab.core.data.values import AcabStatement, AcabValue, Sentence
from acab.error.operator_exception import AcabOperatorException
from acab.core.decorators.util import cache

config = AcabConfig.Get()

logging = root_logger.getLogger(__name__)

Value      = AT.Value
Operator   = AT.Operator
Component  = AT.Component
Container  = AT.Container
PStructure = AT.ProductionStructure


# FIXME this should be an interface
@dataclass(frozen=True)
class ProductionOperator(AcabValue):
    """ The Base Operator Class,
    Provides the way to use other systems and code in Acab

    ContextSet.build uses a class attribute named "sugar" to register a syntax
    sugar for the operator. The string is a pseudo sentence,
    ie: _:==, _:!=, ..
    """

    def __post_init__(self):
        logging.info(f"Building Operator: {self.__class__.__name__}")
        super(ProductionOperator, self).__post_init__()
        object.__setattr__(self, 'name', self.__class__.__name__)
        object.__setattr__(self, 'value', self.name)
        self.data[DS.TYPE_INSTANCE] =  DS.OPERATOR_PRIM

    def __call__(self, *params: List[Value], data=None):
        raise NotImplementedError()

    def copy(self, **kwargs):
        """ Operators by default are immutable, and don't need to duplicate """
        return self

    @property
    @cache
    def op_path(self):
        return self.value

class ActionOperator(ProductionOperator):
    """ Special Operator type which gets passed the semantic system,
    so it can trigger instructions """

    def __call__(self, *params: List[Value], data=None, semSystem=None):
        raise NotImplementedError()


@dataclass(frozen=True)
class ProductionComponent(AcabStatement):
    """ Pairs a an operator with some bindings
    equivalent to a sentence of:
    pc(::production.component).op.$x(::sen).args.[$y.$z].return.$a

    """

    # Sugared: Denotes whether the parse originated from a sugared operator
    # eg: $x ~= /blah/ -> $x
    sugared : bool      = field(default=False)
    rebind  : Value     = field(default=None)

    def __post_init__(self):
        super(ProductionComponent, self).__post_init__()
        assert(isinstance(self.value, Sentence))
        self.data[DS.TYPE_INSTANCE] = DS.COMPONENT_PRIM

    def __len__(self):
        return 1
    def __contains__(self, value):
        return value in self.value or value == rebind

    def to_sentences(self):
        """
        Sentence([op_path].[param1.param2.param3...].result)
        """
        raise NotImplementedError()

    @property
    @cache
    def op(self):
        return self.value

    def bind(self, data) -> Component:
        raise Exception("Deprecated: use acab.modules.values.binding")

    @property
    def has_var(self):
        if self.op.is_var:
            return True
        if any([x.has_var for x in self.params]):
            return True
        if self.type.has_var:
            return True

        return False

@dataclass(frozen=True)
class ProductionContainer(AcabStatement):
    """ Production Container: An applicable statement of multiple component clauses """

    value : List[Sentence] = field(default_factory=list)

    def __post_init__(self):
        super(ProductionContainer, self).__post_init__()
        assert(isinstance(self.value, list))
        self.data[DS.TYPE_INSTANCE] = DS.CONTAINER_PRIM

    def __repr__(self):
        clauses = ";".join([repr(x) for x in self.clauses])
        return "(ProductionContainer:{}:{})".format(self.name,
                                                    clauses)
    @cache
    def __len__(self):
        return len(self.clauses)

    def __iter__(self):
        return iter(self.clauses)

    def __contains__(self, key):
        return key in self.value

    @property
    def clauses(self):
        return self.value

    def bind(self, data) -> Container:
        raise Exception("Deprecated: use acab.modules.values.binding")

    def to_sentences(self):
        """ [ClauseA, ClauseB, ClauseC...] """
        raise NotImplementedError()

@dataclass(frozen=True)
class ProductionStructure(ProductionContainer):
    """
    A ProductionContainer, supplemented by a dictionary
    to group the clauses
    """
    structure: Dict[str, Container] = field(default_factory=dict)

    def __post_init__(self):
        # self.value = []
        super(ProductionContainer, self).__post_init__()
        self.data[DS.TYPE_INSTANCE] = DS.STRUCT_PRIM

        clauses = list(self.structure.values())
        try:
            self.value += clauses
        except FrozenInstanceError as err:
            # Expected
            pass


    @cache
    def __repr__(self):
        actual  = [x for x in self.keys if x in self]
        clauses = ";".join(["({}:{})".format(x,
                                             [str(z) for z in self[x].clauses])
                            for x in actual])

        return "(ProductionStructure:{}:{})".format(self.name, clauses)

    @cache
    def __hash__(self):
        return hash(repr(self))

    def __getitem__(self, key):
        return self.structure[key]


    def __contains__(self, key):
        return key in self.structure and bool(self.structure[key])
    @property
    def keys(self):
        return self.structure.keys()

    def bind(self, data) -> PStructure:
        raise Exception("Deprecated: use acab.modules.values.binding")


    def to_sentences(self):
        """
        [prefix.[ClauseA],
         prefix.[ClauseB..],
         prefixB.[ClauseX],
         prefixB.[Clause,Y],
         prefixC.[..]
        ..]
        """
        raise NotImplementedError()
