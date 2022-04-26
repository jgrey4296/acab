"""
Classes for defining types
"""
import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from dataclasses import dataclass, field, InitVar

from acab.core.config.config import AcabConfig
from acab.core.value.value import AcabValue
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.error.parse import AcabParseException
from acab.modules.analysis.typing import exceptions as TE
from acab.modules.analysis.typing.util import (SUM_DEFINITION, TYPE_DEF_S,
                                               TYPE_DEFINITION, OPERATOR_DEFINITION)

from .acab_type import TypeStatement

logging         = logmod.getLogger(__name__)
config          = AcabConfig()

PRIMITIVE_S     = config.prepare("Typing.Primitives", "PRIMITIVE")()
TYPE_INSTANCE_S = config.prepare("Parse.Structure", "TYPE_INSTANCE")()
NAME_S          = config.prepare("Parse.Structure", "NAME")()

@dataclass(frozen=True)
class TypeDefinition(TypeStatement):
    """ Defines the Structure of a Product type """

    def __post_init__(self):
        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x ::String, .b.$c ::Num, .d!$e ::Location }
        """
        super(TypeDefinition, self).__post_init__()
        # The name is the location. eg: .types.person
        assert isinstance(self.structure, list)
        assert all([isinstance(x, Sentence) for x in self.structure]), self.structure

        self.data[TYPE_INSTANCE_S] = TYPE_DEFINITION


    def __eq__(self, other):
        path_eq = self.path == other.path
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


    def __post_init__(self):
        # Flatten Product Types out of Structure:
        # TODO: improve this
        super(SumTypeDefinition, self).__post_init__()
        # flat_structure = []
        # for sen in self.structure:
        #     prefix = Sentence(sen.words[:-1] + [sen.words[-1].to_word()])
        #     flat_structure.append(prefix)
        #     flat_structure += [Sentence(prefix.words + x.words) for x in sen[-1].structure]

        # self.value = flat_structure

        self.data[TYPE_INSTANCE_S] = SUM_DEFINITION

@dataclass(frozen=True)
class OperatorDefinition(TypeDefinition):
    """ Defines the type signature of an operator"""

    sugar_syntax : str = field(default=None)

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

    def __post_init__(self):
        super(TypeClass, self).__post_init__()
