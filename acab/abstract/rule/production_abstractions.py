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

from acab.abstract.core.core_abstractions import AcabValue, AcabStatement, Sentence

util = AcabConfig.Get()

OPERATOR_S            = util.value("Value.Structure", "OPERATOR")
AT_BIND_S             = util.value("Value.Structure", "AT_BIND")
CONTAINER_TYPE_PRIM_S = util.value("Type.Primitive", "CONTAINER")
COMPONENT_TYPE_PRIM_S = util.value("Type.Primitive", "COMPONENT")
OPERATOR_TYPE_PRIM_S  = util.value("Type.Primitive", "OPERATOR")

logging = root_logger.getLogger(__name__)

class ProductionOperator(AcabValue):
    """ The Base Operator Class """

    def __init__(self):
        super().__init__(self.__class__.__name__, data={TYPE_INSTANCE: OPERATOR_TYPE_PRIM_S})

    def __call__(self, *params, data=None, engine=None):
        raise NotImplementedError()

    @property
    def op_path(self):
        return self.value



@dataclass(frozen=True)
class ProductionComponent(AcabStatement):
    """ Pairs a an operator with some bindings """

    # Sugared: Denotes whether the parse originated from a sugared operator
    # eg: $x ~= /blah/ -> $x
    sugared : bool      = False
    rebind  : AcabValue = field(default=None)

    def __post_init__(self):
        super(ProductionComponent, self).__post_init__()
        assert(isinstance(self.value, Sentence))
        self.data[TYPE_INSTANCE] = Sentence.build([COMPONENT_TYPE_PRIM_S])


    @property
    def op(self):
        return self.value

    @property
    def var_set(self):
        obj = super(ProductionComponent, self).var_set
        for p in self._params:
            tempobj = p.var_set
            obj['in'].update(tempobj['in'])
            obj['in'].update(tempobj['out'])

        if self._rebind is not None:
            obj['out'].add(self._rebind)
        return obj


    @property
    def rebind(self):
        return self._rebind

    def get_params(self, data):
        """ Output a list of bindings from this action,
        Unwraps as necessary
        """
        assert(isinstance(data, dict))
        output = []
        # TODO: enable currying
        for x in self._params:
            if isinstance(x, Sentence):
                output.append(x.bind(data))
            elif isinstance(x, list):
                output.append([y.bind(data) for y in x])
            elif isinstance(x, AcabValue) and x.is_var:
                assert(x.value in data)
                if x.is_at_var:
                    output.append(data[AT_BIND_S + x.value])
                elif isinstance(data[x.value], list):
                    # TODO does this need to unwrap all list values?
                    output.append(data[x.value])
                else:
                    output.append(data[x.value].value)
            else:
                output.append(x.value)
        return output

    def __refine_op_func(self, op_path):
        """ Replace the current op func set with a specific
        op func, used for type refinement """
        raise DeprecationWarning()


    def to_abstract_sentences(self, target=None):
        """

        """
        raise NotImplementedError()

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
        verified._value = op

        return verified


@dataclass(frozen=True)
class ProductionContainer(AcabStatement):
    """ Production Container: An applicable statement of multiple component clauses """
    def __post_init__(self):
        super(ProductionContainer, self).__post_init__()
        self.data[TYPE_INSTANCE] = CONTAINER_TYPE_PRIM_S

    def __len__(self):
        return len(self.clauses)

    def __iter__(self):
        return self.clauses.iter()

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


    def to_abstract_sentences(self, target=None):
        return [y for x in self.clauses for y in x.to_abstract_sentences()]



@dataclass(frozen=True)
class ProductionStructure(ProductionContainer):
    """
    A ProductionContainer, supplemented by a dictionary
    to group the clauses
    """
    structure: Dict[str, ProductionContainer] = field(default_factory=dict)

    def __post_init__(self):
        super(ProductionComponent, self).__post_init__()
        self.data[TYPE_INSTANCE] = CONTAINER_TYPE_PRIM_S
        assert(not bool(self.clauses))

        clauses = list(self.structure.values())
        self.clauses += clauses

    @property
    def keys(self):
        return self.structure.keys()

    def __getitem__(self, key):
        return self.structure[key]
