"""
Classes for defining types
"""
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.interfaces.value as VI
from acab import types as AT
from acab import AcabConfig
from acab.core.defaults import value_keys as DS
from acab.core.value.instruction import Instruction
from acab.error.parse import AcabParseException
from acab.modules.analysis.typing import exceptions as TE
from acab.modules.analysis.typing.util import (OPERATOR_DEFINITION,
                                               SUM_DEFINITION, TYPE_CLASS,
                                               TYPE_DEF_S, TYPE_DEFINITION)

logging         = logmod.getLogger(__name__)
config          = AcabConfig()

PRIMITIVE_S     = config.prepare("Typing.Primitives", "PRIMITIVE")()
TYPE_INSTANCE_S = config.prepare("Parse.Structure", "TYPE_INSTANCE")()
NAME_S          = config.prepare("Parse.Structure", "NAME")()

Sen = AT.Sentence

@dataclass(frozen=True)
class TypeStatement(Instruction, VI.Instruction_i):
    # TODO: change value to a config value

    value : list[Sen] = field(default_factory=list)
    name  : str       = field(default="|∀σ|")
    _path : Sen       = field(default=None)

    def __post_init__(self, *args, **kwargs):
        pass

    def to_sentences(self):
        return self.structure[:]

    @property
    def head(self):
        return self._path[-1]

    @property
    def vars(self):
        return self.params

    @property
    def structure(self):
        return self.value
@dataclass(frozen=True)
class TypeDefinition(TypeStatement):
    """ Defines the Structure of a Product type """

    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: TYPE_DEFINITION}

    def __post_init__(self):
        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x ::String, .b.$c ::Num, .d!$e ::Location }
        """
        super(TypeDefinition, self).__post_init__()
        # The name is the location. eg: .types.person
        assert isinstance(self.structure, list)
        assert all([isinstance(x, VI.Sentence_i) for x in self.structure]), self.structure


    def __eq__(self, other):
        path_eq = self._path == other._path
        structure_len = len(self.structure) == len(other.structure)
        structure_eq = all([x == y for x,y in zip(self.structure, other.structure)])

        return path_eq and structure_len and structure_eq


    def __contains__(self, key):

        return key in self.structure

    def __len__(self):
        return len(self.structure)

    def __repr__(self):
        return f"<TypeDefinition {self.name} ({len(self.structure)})>"

# TODO Factor these into typedef: ###############################################
@dataclass(frozen=True)
class SumTypeDefinition(TypeDefinition):
    """ Defines a Sum Type  """

    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: SUM_DEFINITION}

    def __post_init__(self):
        # Flatten Product Types out of Structure:
        # TODO: improve this
        super(SumTypeDefinition, self).__post_init__()
        # flat_structure = []
        # for sen in self.structure:
        #     prefix = VI.Sentence_i(sen.words[:-1] + [sen.words[-1].to_word()])
        #     flat_structure.append(prefix)
        #     flat_structure += [VI.Sentence_i(prefix.words + x.words) for x in sen[-1].structure]

        # self.value = flat_structure


@dataclass(frozen=True)
class OperatorDefinition(TypeDefinition):
    """ Defines the type signature of an operator"""

    sugar_syntax : str                      = field(default=None)
    _defaults    : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: OPERATOR_DEFINITION}

    def __post_init__(self):
        """ The name of an operator and its type signature,
        with the binding to a ProductionOperator that is
        syntax sugared, and its inline place"""
        # eg: operator.+.$x(::num).$y(::num).$z(::num).num_plus
        super(OperatorDefinition, self).__post_init__()

    def __hash__(self):
        return hash(str(self))

@dataclass(frozen=True)
class TypeClass(TypeDefinition):
    """ Definition of a coherent collection of functions """

    _defaults : ClassVar[dict[str, Any]] = {DS.TYPE_INSTANCE: TYPE_CLASS}

    def __post_init__(self):
        super(TypeClass, self).__post_init__()
