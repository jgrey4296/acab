"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators

Structure is:
ProductionOperator  : The implementation
ProductionComponent : Pairs the Operator with bindings
ProductionContainer : Groups Components together

"""
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace, FrozenInstanceError
from fractions import Fraction
from re import Pattern
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence,
                    Tuple, TypeVar, TypeAlias, cast)
from uuid import UUID, uuid1
from weakref import ref

from acab.error.protocol import AcabProtocolError as APE
from acab import types as AT
import acab.core.defaults.value_keys as DS
from acab.core.config.config import AcabConfig
from acab.core.util.decorators.util import cache
import acab.interfaces.value as VI
from acab.core.util.part_implementations.instruction import InstructionProtocolsImpl
from acab.core.util.part_implementations.value import ValueProtocolsImpl
from acab.interfaces.value import ValueFactory as VF
from acab.core.value.value_meta import ValueMeta

logging = logmod.getLogger(__name__)
config = AcabConfig()

value_meta = config.prepare("Imports.Targeted", "value_meta", actions=[config.actions_e.IMCLASS], default=ValueMeta)()

Value_A       : TypeAlias = AT.Value
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
Operator      : TypeAlias = AT.Operator
Component     : TypeAlias = AT.Component
Container     : TypeAlias = AT.Container
PStructure    : TypeAlias = AT.ProductionStructure
ValueData     : TypeAlias = AT.ValueData

T = TypeVar('T')

@APE.assert_implements(VI.Instruction_i, exceptions=["to_sentences"])
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

    @staticmethod
    def from_sentences(self, sens:list[Sen_A]) -> list[Instruction_A]:
        raise NotImplementedError()

    def do_break(self) -> None: pass

    @property
    def should_break(self) -> bool:
        return bool(self.breakpoint)

@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True, repr=False)
class ProductionComponent(Instruction):
    """ Pairs a an operator with some bindings
    equivalent to a sentence of:
    pc(::production.component).op.$x(::sen).args.[$y.$z].return.$a

    """
    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.COMPONENT_PRIM}

    # Sugared: Denotes whether the parse originated from a sugared operator
    # eg: $x ~= /blah/ -> $x
    sugared : bool           = field(default=False)
    # TODO shift this into params
    rebind  : 'None|Value_A' = field(default=None)


    def __post_init__(self):
        raise DeprecationWarning("Use a sentence instead")

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

    def to_sentences(self):
        """ [ClauseA, ClauseB, ClauseC...] """
        base = VI.ValueFactory.sen()
        sens = []
        sens.append(base << self.__class__.__name__ << self.name << self.type << str(self.uuid))
        for clause in self.clauses:
            if isinstance(clause, VI.Sentence_i):
                sens.append(clause)
            elif isinstance(clause, Instruction):
                sens += clause.to_sentences()

        sens.append(base << "end" << str(self.uuid))
        return sens


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

    def to_sentences(self):
        """
        <section>
        <clause>
        <clause>
        <section>
        <clause>
        ...
        """
        base = VI.ValueFactory.sen()
        sens = []
        sens.append(base << self.__class__.__name__ << self.name << self.type << str(self.uuid))
        for key in self.keys:
            clause = self[key]
            if isinstance(clause, VI.Sentence_i):
                sens.append(clause)
            elif isinstance(clause, Instruction):
                sens += clause.to_sentences()

        sens.append(base << "end" << str(self.uuid))
        return sens

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
