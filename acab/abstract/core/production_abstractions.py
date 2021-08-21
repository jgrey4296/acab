"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators

Structure is:
ProductionOperator  : The implementation
ProductionComponent : Pairs the Operator with bindings
ProductionContainer : Groups Components together

"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from re import Pattern
from uuid import uuid1, UUID
from weakref import ref
import logging as root_logger

from acab.error.acab_operator_exception import AcabOperatorException
from acab.abstract.config.config import AcabConfig

from acab.abstract.core.values import AcabValue, AcabStatement, Sentence
import acab.abstract.core.default_structure as DS

config = AcabConfig.Get()

logging = root_logger.getLogger(__name__)

Operator  = 'ProductionOperator'
Component = 'ProductionComponent'
Container = 'ProductionContainer'
Structure = 'ProductionStructure'


# TODO should this be an interface?
@dataclass
class ProductionOperator(AcabValue):
    """ The Base Operator Class,
    Provides the way to use other systems and code in Acab
    """

    def __post_init__(self):
        logging.info(f"Building Operator: {self.__class__.__name__}")
        super(ProductionOperator, self).__post_init__()
        object.__setattr__(self, 'name', self.__class__.__name__)
        object.__setattr__(self, 'value', self.name)
        self.data[DS.TYPE_INSTANCE] =  DS.OPERATOR_PRIM

    def __call__(self, *params: List[AcabValue], data=None):
        raise NotImplementedError()

    @property
    def op_path(self):
        return self.value

class ActionOperator(ProductionOperator):
    """ Special Operator type which gets passed the semantic system,
    so it can trigger instructions """

    def __call__(self, *params: List[AcabValue], data=None, semSystem=None):
        raise NotImplementedError()


@dataclass
class ProductionComponent(AcabStatement):
    """ Pairs a an operator with some bindings """

    # Sugared: Denotes whether the parse originated from a sugared operator
    # eg: $x ~= /blah/ -> $x
    sugared : bool      = False
    rebind  : AcabValue = field(default=None)

    def __post_init__(self):
        super(ProductionComponent, self).__post_init__()
        assert(isinstance(self.value, Sentence))
        self.data[DS.TYPE_INSTANCE] = DS.COMPONENT_PRIM


    @property
    def op(self):
        return self.value

    @property
    def var_set(self):
        obj = super(ProductionComponent, self).var_set
        obj['in'].update(self.params)

        if self.rebind is not None:
            obj['out'].add(self.rebind)
        return obj

    def verify(self, ctx=None, engine=None):
        """ Verify the Component, retrieving the operator from the engine
        if necessary """
        # $op -> retrieve from ctx
        op = self.op
        if len(op) == 1 and op[0].is_var and ctx is not None:
            op = ctx[op.value]

        if not isinstance(op, ProductionOperator) and engine is not None:
            op = engine.get_operator(op)

        # TODO: op's should be able to be Components and Containers as well?
        if not isinstance(op, ProductionOperator) and engine is not None:
            raise AcabOperatorException(op)

        # TODO make op cached, rather than this:
        verified = self.copy()
        verified.value = op

        return verified


    def bind(self, data) -> Component:
        # Bind params / operator
        if self.op.is_var and self.op.value in data:
            bound_op = data[self.op.value]
        else:
            bound_op = self.op.copy()

        bound_params = [x.bind(data) for x in self.params]

        return self.copy(value=bound_op, params=bound_params)

@dataclass
class ProductionContainer(AcabStatement):
    """ Production Container: An applicable statement of multiple component clauses """
    def __post_init__(self):
        super(ProductionContainer, self).__post_init__()
        assert(isinstance(self.value, list))
        self.data[DS.TYPE_INSTANCE] = DS.CONTAINER_PRIM

    def __len__(self):
        return len(self.clauses)

    def __iter__(self):
        return iter(self.clauses)

    @property
    def clauses(self):
        return self.value

    @property
    def var_set(self):
        """ Return a set of all bindings this container utilizes """
        # ie: Query(a.b.$x? a.q.$w?).var_set -> {'in': [], 'out': [x,w]}
        # Action(+(a.b.$x), -(a.b.$w)).var_set -> {'in': [x,w], 'out': []}
        obj = super(ProductionContainer, self).var_set
        for p in self.clauses:
            if isinstance(p, AcabValue):
                tempobj = p.var_set
                obj['in'].update(tempobj['in'])
                obj['out'].update(tempobj['out'])
        return obj


    def bind(self, data) -> Container:
        # Bind params,
        # then Bind each clause separately,
        bound_clauses = [x.bind(data) for x in self.value]
        bound_params = [x.bind(data) for x in self.params]
        return self.copy(value=bound_clauses, params=bound_params)


@dataclass
class ProductionStructure(ProductionContainer):
    """
    A ProductionContainer, supplemented by a dictionary
    to group the clauses
    """
    structure: Dict[str, ProductionContainer] = field(default_factory=dict)

    def __post_init__(self):
        self.value = []
        super(ProductionContainer, self).__post_init__()
        # TODO make this a structure prim?
        self.data[DS.TYPE_INSTANCE] = DS.STRUCT_PRIM

        clauses = list(self.structure.values())
        self.value += clauses

    @property
    def keys(self):
        return self.structure.keys()

    def __getitem__(self, key):
        return self.structure[key]

    def bind(self, data) -> Structure:
        # Bind params,
        bound_params = [x.bind(data) for x in self.params]
        # Bind clauses
        bound_clauses = [x.bind(data) for x in self.clauses]
        # Bind sub containers
        bound_struct = {x: y.bind(data) for x,y in self.structure.items()}

        return self.copy(value=bound_clauses, params=bound_params, structure=bound_struct)

    def __contains__(self, key):
        return key in self.structure
