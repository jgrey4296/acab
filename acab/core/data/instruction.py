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
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence,
                    Tuple, TypeVar, TypeAlias, cast)
from uuid import UUID, uuid1
from weakref import ref

from acab.error.protocol import AcabProtocolError as APE
from acab import types as AT
import acab.core.data.default_structure as DS
from acab.core.config.config import AcabConfig
from acab.core.data.value import AcabValue
from acab.core.data.sentence import Sentence
from acab.error.operator import AcabOperatorException
from acab.core.decorators.util import cache
import acab.interfaces.value as VI
from acab.core.data.sub_implementations.sentence import SentenceProtocolsImpl
from acab.core.data.sub_implementations.value import ValueProtocolsImpl
from acab.core.data.factory import ValueFactory as VF

config = AcabConfig()

logging = root_logger.getLogger(__name__)

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
class Instruction(SentenceProtocolsImpl, VI.Instruction_i):
    """ Instruction functions the same as AcabValue,
    but provides specific functionality for converting to/from sentences
    """


    @classmethod
    def build(cls, value:T, /, *,
              name:None|str=None,
              data:None|dict[ValueData, Any]=None,
              params:None|list['Value_A|str']=None,
              tags:None|list['Value_A|str']=None,
              _type:'None|str|Sen_A'=None,
              **kwargs) -> Value_A:

        if name is None:
            name = f"{cls.__module__}.{cls.__qualname__}"

        _data        = VF._build_data_and_type(data, _type or DS.CONTAINER_PRIM)
        tags, params = VF._build_tags_and_params(tags, params)

        new_obj = cls(value, name=name, data=_data, tags=tags, params=params, **kwargs)
        # Pre-initialisation, the VF value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return cast(VI.Instruction_i, new_obj)

    def __repr__(self):
        return "<{}::{}>".format(self.name, str(self.type))

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

@APE.assert_implements(VI.Operator_i, exceptions=["__call__"])
class ProductionOperator(ValueProtocolsImpl, VI.Operator_i):
    """ The Base Operator Class,
    Provides the way to use other systems and code in Acab

    ContextSet.build uses a class attribute named "sugar" to register a syntax
    sugar for the operator. The string is a pseudo sentence,
    ie: _:==, _:!=, ..
    """


    @classmethod
    def build(cls, value:T=None, /, *,
              name:None|str=None,
              data:None|dict[ValueData, Any]=None,
              params:None|list['Value_A|str']=None,
              tags:None|list['Value_A|str']=None,
              _type:'None|str|Sen_A'=None,
              **kwargs) -> Value_A:
        logging.info(f"Building Operator: {cls.__class__.__name__}")
        if name is None:
            name = f"{cls.__module__}.{cls.__qualname__}"

        _data        = VF._build_data_and_type(data, _type or DS.OPERATOR_PRIM)
        tags, params = VF._build_tags_and_params(tags, params)

        new_obj = cls(value, name=name, data=_data, tags=tags, params=params, **kwargs)
        # Pre-initialisation, the VF value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return cast(VI.Operator_i, new_obj)

    def copy(self, **kwargs):
        """ Operators by default are immutable, and don't need to duplicate """
        return self

    @property #type:ignore
    @cache
    def op_path(self):
        return self.value


    # __lt__ __str__ apply_params apply_tags build build_sen has_tag
    # has_var is_at_var is_var key type


@APE.assert_implements(VI.Operator_i, exceptions=["__call__"])
class ActionOperator(ValueProtocolsImpl, VI.Action_i):
    """ Special Operator type which gets passed the semantic system,
    so it can trigger instructions """
    @classmethod
    def build(cls, value:T=None, /, *,
              name:None|str=None,
              data:None|dict[ValueData, Any]=None,
              params:None|list['Value_A|str']=None,
              tags:None|list['Value_A|str']=None,
              _type:'None|str|Sen_A'=None,
              **kwargs) -> Value_A:

        logging.info(f"Building Action: {cls.__class__.__name__}")
        if name is None:
            name = f"{cls.__module__}.{cls.__qualname__}"

        _data        = VF._build_data_and_type(data, _type or DS.OPERATOR_PRIM)
        tags, params = VF._build_tags_and_params(tags, params)

        new_obj = cls(value, name=name, data=_data, tags=tags, params=params, **kwargs)
        # Pre-initialisation, the VF value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return cast(VI.Action_i, new_obj)


    def copy(self, **kwargs):
        """ Operators by default are immutable, and don't need to duplicate """
        return self


@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True)
class ProductionComponent(Instruction):
    """ Pairs a an operator with some bindings
    equivalent to a sentence of:
    pc(::production.component).op.$x(::sen).args.[$y.$z].return.$a

    """

    # Sugared: Denotes whether the parse originated from a sugared operator
    # eg: $x ~= /blah/ -> $x
    sugared : bool           = field(default=False)
    # TODO shift this into params
    rebind  : 'None|Value_A' = field(default=None)

    @classmethod
    def build(cls, value:T, /, *,
              name:None|str=None,
              data:None|dict[ValueData, Any]=None,
              params:None|list['Value_A|str']=None,
              tags:None|list['Value_A|str']=None,
              _type:'None|str|Sen_A'=None,
              **kwargs) -> Value_A:

        if name is None:
            name = f"{cls.__module__}.{cls.__qualname__}"

        assert(isinstance(value, VI.Sentence_i))

        _data        = VF._build_data_and_type(data, _type or DS.COMPONENT_PRIM)
        tags, params = VF._build_tags_and_params(tags, params)

        new_obj = cls(value, name=name, data=_data, tags=tags, params=params, **kwargs)
        # Pre-initialisation, the VF value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return new_obj


    def __len__(self):
        return 1

    def __contains__(self, value):
        return value in self.value or value == rebind

    def __repr__(self):
        return super().__repr__()

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
        return VF.sen(words, data=self.data.copy(), name="ProductionComponent")

    @staticmethod
    def from_sentences(sens):
        result = []
        for sen in sens:
            if sen.name != "ProductionComponent":
                continue
            if "Operator" not in sen:
                continue

            comp   = sen['Operator'][:]
            params = sen['Params'].words if 'Params' in sen else []
            rebind = sen['Rebind'].copy(name=None) if 'Rebind' in sen else None
            result.append(ProductionComponent.build(comp, params=params, rebind=rebind))

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
    """ Production Container: An applicable statement of multiple component clauses """

    @classmethod
    def build(cls, value:T, /, *,
              name:None|str=None,
              data:None|dict[ValueData, Any]=None,
              params:None|list['Value_A|str']=None,
              tags:None|list['Value_A|str']=None,
              _type:'None|str|Sen_A'=None,
              **kwargs) -> Value_A:

        if name is None:
            name = f"{cls.__module__}.{cls.__qualname__}"

        assert(isinstance(value, list))

        _data        = VF._build_data_and_type(data, _type or DS.CONTAINER_PRIM)
        tags, params = VF._build_tags_and_params(tags, params)

        new_obj = cls(value, name=name, data=_data, tags=tags, params=params, **kwargs)
        # Pre-initialisation, the VF value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return cast(VI.Instruction_i, new_obj)

    def __repr__(self):
        # clauses = ";".join([repr(x) for x in self.clauses])
        return "<{}::{}>".format(self.name, str(self.type))
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
            if isinstance(clause, Sentence):
                words.append(clause)
            elif isinstance(clause, Instruction):
                words += clause.to_sentences()

        return [VF.sen(words,
                       data=self.data.copy(),
                       name="ProductionContainer")]



@APE.assert_implements(VI.Instruction_i)
@dataclass(frozen=True)
class ProductionStructure(ProductionContainer):
    """
    A ProductionContainer, supplemented by a dictionary
    to group the clauses
    """
    structure: dict[str, Container] = field(default_factory=dict)

    @classmethod
    def build(cls, value:dict[str, T], /, *,
              name:None|str=None,
              data:None|dict[ValueData, Any]=None,
              params:None|list['Value_A|str']=None,
              tags:None|list['Value_A|str']=None,
              _type:'None|str|Sen_A'=None,
              **kwargs) -> Value_A:

        if name is None:
            name = f"{cls.__module__}.{cls.__qualname__}"

        assert(isinstance(value, dict))
        clauses = list(value.values())
        _data        = VF._build_data_and_type(data, _type or DS.STRUCT_PRIM)
        tags, params = VF._build_tags_and_params(tags, params)


        new_obj = cls(clauses, structure=value, name=name, data=_data, tags=tags, params=params, **kwargs)
        # Pre-initialisation, the VF value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return cast(VI.Instruction_i, new_obj)

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
        clauses = []
        for key in self.keys():
            words = []
            clause = self[key]
            if isinstance(clause, Sentence):
                words.append(clause)
            elif isinstance(clause, Instruction):
                words += clause.to_sentences()

            clauses.append(VF.sen(words, name=key))

        return [VF.sen([clauses],
                       data=self.data.copy(),
                       name="ProductionStructure")]
