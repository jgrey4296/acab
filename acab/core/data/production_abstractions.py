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

config = AcabConfig.Get()

logging = root_logger.getLogger(__name__)

Value      = AT.Value
Operator   = AT.Operator
Component  = AT.Component
Container  = AT.Container
PStructure = AT.ProductionStructure


# TODO should this be an interface?
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
    """ Pairs a an operator with some bindings """

    # Sugared: Denotes whether the parse originated from a sugared operator
    # eg: $x ~= /blah/ -> $x
    sugared : bool      = field(default=False)
    rebind  : Value     = field(default=None)

    def __post_init__(self):
        super(ProductionComponent, self).__post_init__()
        assert(isinstance(self.value, Sentence))
        self.data[DS.TYPE_INSTANCE] = DS.COMPONENT_PRIM


    @property
    @cache
    def op(self):
        return self.value

    def bind(self, data) -> Component:
        # Bind params / operator
        if self.op.is_var and self.op.value in data:
            bound_op = data[self.op.value]
        else:
            bound_op = self.op.copy()

        bound_params = [x.bind(data) for x in self.params]

        return self.copy(value=bound_op, params=bound_params)

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

    value : List = field(default_factory=list)

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

    @property
    def clauses(self):
        return self.value

    def bind(self, data) -> Container:
        # Bind params,
        # then Bind each clause separately,
        bound_clauses = [x.bind(data) for x in self.value]
        bound_params  = [x.bind(data) for x in self.params]
        return self.copy(value=bound_clauses, params=bound_params)


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

    @property
    def keys(self):
        return self.structure.keys()

    def __getitem__(self, key):
        return self.structure[key]

    def bind(self, data) -> PStructure:
        # Bind params,
        bound_params  = [x.bind(data) for x in self.params]
        # Bind clauses
        bound_clauses = [x.bind(data) for x in self.clauses]
        # Bind sub containers
        bound_struct  = {x: y.bind(data) for x,y in self.structure.items()}

        return self.copy(value=bound_clauses, params=bound_params, structure=bound_struct)

    def __contains__(self, key):
        return key in self.structure and bool(self.structure[key])
