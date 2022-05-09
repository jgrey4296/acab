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
import acab.core.value.default_structure as DS
from acab.core.config.config import AcabConfig
from acab.core.util.decorators.util import cache
import acab.interfaces.value as VI
from acab.core.util.part_implementations.instruction import InstructionProtocolsImpl
from acab.core.util.part_implementations.value import ValueProtocolsImpl
from acab.core.value.factory import ValueFactory as VF
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

@APE.assert_implements(VI.Instruction_i)
class Instruction(InstructionProtocolsImpl, VI.Instruction_i, metaclass=value_meta):
    """ Instruction functions the same as AcabValue,
    but provides specific functionality for converting to/from sentences
    """

    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: DS.CONTAINER_PRIM}

    @classmethod
    def _preprocess(cls, *args, **kwargs):
        assert(isinstance(args[0], (list, VI.Sentence_i, Iterable)))
        return args[0]

    def copy(self, **kwargs) -> Instruction_A:
        return replace(self, uuid=uuid1(), **kwargs)

    def to_word(self) -> Value_A:
        """ Convert a Statement to just an AcabValue, of it's name """
        new_data = {}
        new_data.update(self.data)
        del new_data[DS.TYPE_INSTANCE]
        simple_value = VF.value(self.name, data=new_data, tags=self.tags)
        return simple_value

    def to_sentences(self) -> list[VI.Sentence_i]:
        return []

    @staticmethod
    def from_sentences(self, sens:list[Sen_A]) -> list[Instruction_A]:
        return cast(list[Instruction_A], sens)



    def do_break(self) -> None: pass

    @property
    def should_break(self) -> bool:
        return bool(self.breakpoint)

@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True)
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
        assert(isinstance(self.value, VI.Sentence_i)), self.value

    def __len__(self):
        return 1

    def __contains__(self, value):
        return value in self.value or value == rebind

    def to_sentences(self):
        """
        Sentence([op_path].[param1.param2.param3...].result)
        """
        words = []
        words.append(self.value.copy(name="Operator"))
        if bool(self.params):
            words.append(VF.sen(self.params, name="Params"))
        if self.rebind:
            words.append(self.rebind.copy(name="Rebind"))
        return VF.sen(words, data=self.data.copy(), name=self.__class__.__name__)

    @staticmethod
    def from_sentences(sens):
        result = []
        for sen in sens:
            if sen.name != ProductionComponent.__name__:
                continue
            if "Operator" not in sen:
                continue

            comp   = sen['Operator'][:]
            params = sen['Params'].words if 'Params' in sen else []
            rebind = sen['Rebind'].copy(name=None) if 'Rebind' in sen else None
            result.append(ProductionComponent(comp, params=params, rebind=rebind))

        return result

    @property #type:ignore
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

@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True)
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

    def bind(self, data) -> Container:
        raise Exception("Deprecated: use acab.modules.values.binding")


    def to_sentences(self):
        """ [ClauseA, ClauseB, ClauseC...] """
        words = []
        for clause in self.clauses:
            if isinstance(clause, VI.Sentence_i):
                words.append(clause)
            elif isinstance(clause, Instruction):
                words += clause.to_sentences()

        return [VF.sen(words,
                       data=self.data.copy(),
                       name=self.__class__.__name__)]



@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True)
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
        actual  = [x for x in self.keys if x in self]
        clauses = ";".join(["({}:{})".format(x,
                                             [str(z) for z in self[x].clauses])
                            for x in actual])

        return "<ProductionStructure:{}:{}>".format(self.name, clauses)

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
        clauses = []
        for key in self.keys():
            words = []
            clause = self[key]
            if isinstance(clause, VI.Sentence_i):
                words.append(clause)
            elif isinstance(clause, Instruction):
                words += clause.to_sentences()

            clauses.append(VF.sen(words, name=key))

        return [VF.sen([clauses],
                       data=self.data.copy(),
                       name=self.__class__.__name__)]


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
