"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators

Structure is:
ProductionOperator  : The implementation
ProductionComponent : Pairs the Operator with bindings
ProductionContainer : Groups Components together

"""
##-- imports
from __future__ import annotations

import logging as logmod
from copy import deepcopy
from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from fractions import Fraction
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, Sequence, Tuple,
                    TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab
import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.util.decorators.util import cache
from acab.core.value.part_implementations.instruction import \
    InstructionProtocolsImpl
from acab.core.value.part_implementations.value import ValueProtocolsImpl
from acab.core.value.value import ValueMeta
from acab.interfaces.value import ValueFactory as VF
from acab_config import AcabProtocolError as APE

if TYPE_CHECKING:
    Value_A       : TypeAlias = AT.Value
    Sen_A         : TypeAlias = AT.Sentence
    Instruction_A : TypeAlias = AT.Instruction
    Operator      : TypeAlias = AT.Operator
    Container     : TypeAlias = AT.Container
    PStructure    : TypeAlias = AT.ProductionStructure
    ValueData     : TypeAlias = AT.ValueData

##-- end imports

T = TypeVar('T')

logging    = logmod.getLogger(__name__)
config     = acab.config
# TODO import class
value_meta = config.on_fail().imports.specific.value_meta(wrapper="import_class")


@APE.assert_implements(VI.Instruction_i)
class Instruction(InstructionProtocolsImpl, VI.Instruction_i, metaclass=value_meta):
    """ Instruction functions the same as AcabValue,
    but provides specific functionality for converting to/from sentences
    """

    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.CONTAINER_PRIM}

    @classmethod
    def _preprocess(cls, *args, **kwargs):
        assert(isinstance(args[0], (VI.Sentence_i, Iterable)))
        return args[0]

    def __post_init__(self):
        if self.data[DS.BIND] != False:
            raise TypeError("Sentences Shouldn't be variables")

    def copy(self, **kwargs) -> Instruction_A:
        return replace(self, uuid=uuid1(), **kwargs)

    def to_word(self) -> Value_A:
        """ Convert a Statement to just an AcabValue, of it's name """
        new_data = {}
        new_data.update(self.data)
        del new_data[DS.TYPE_INSTANCE]
        simple_value = VF.value(self.name, data=new_data, tags=self.tags)
        return simple_value

@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True, repr=False)
class ProductionContainer(Instruction):
    """ Production Container: An applicable statement of multiple component clauses
    Clauses can be sentences, or ProductionComponents
    """
    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.CONTAINER_PRIM}

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

    def __lshift__(self, other):
        if other is None:
            return self

        assert(isinstance(other, VI.Sentence_i))
        return self.copy(value=self.value + [other])

@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True, repr=False)
class ProductionStructure(ProductionContainer):
    """
    A ProductionContainer, supplemented by a dictionary
    to group the clauses
    """
    structure: dict[str, Container] = field(init=False, default_factory=dict)

    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.STRUCT_PRIM}


    @classmethod
    def _preprocess(cls, *args, **kwargs):
        value = args[0]
        assert(isinstance(value, Iterable))
        # TODO improve this
        return [ProductionContainer([], name=x) if not isinstance(x, ProductionContainer) else x for x in value]

    def __post_init__(self):
        self.structure.update({x.key() : x for x in self.value})


    @cache
    def __repr__(self):
        clause_keys = [x for x in self.keys if bool(self[x])]

        return "<{}::{} : {{{}}}>".format(self.name,
                                      str(self.type),
                                      ";".join(clause_keys))

    @cache
    def __hash__(self):
        return hash(repr(self))

    def __getitem__(self, key):
        return self.structure[key]

    def __contains__(self, key):
        return key in self.structure

    @property
    def keys(self):
        return self.structure.keys()


@APE.assert_implements(VI.Operator_i, exceptions=["__call__"])
class ProductionOperator(ValueProtocolsImpl, VI.Operator_i, metaclass=value_meta):
    """ The Base Operator Class,
    Provides the way to use other systems and code in Acab

    ContextSet uses a class attribute named "sugar" to register a syntax
    sugar for the operator. The string is a pseudo sentence,
    ie: _:==, _:!=, ..
    """

    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.OPERATOR_PRIM}

    def copy(self, **kwargs):
        """ Operators by default are immutable, and don't need to duplicate """
        return self

    @property #type:ignore
    @cache
    def op_path(self):
        return self.value


@APE.assert_implements(VI.Operator_i, exceptions=["__call__"])
class ActionOperator(ValueProtocolsImpl, VI.Action_i, metaclass=value_meta):
    """ Special Operator type which gets passed the semantic system,
    so it can trigger instructions """
    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.OPERATOR_PRIM}

    def copy(self, **kwargs):
        """ Operators by default are immutable, and don't need to duplicate """
        return self
