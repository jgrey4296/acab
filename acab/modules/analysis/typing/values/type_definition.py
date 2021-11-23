"""
Classes for defining types
"""
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from dataclasses import dataclass, field, InitVar

from acab.core.config.config import AcabConfig
from acab.core.data.values import AcabValue, Sentence
from acab.error.parse_exception import AcabParseException
from acab.modules.analysis.typing import type_exceptions as TE
from acab.modules.analysis.typing.util import (SUM_DEFINITION, TYPE_DEF_S,
                                               TYPE_DEFINITION, OPERATOR_DEFINITION)

from .acab_type import TypeStatement

logging         = root_logger.getLogger(__name__)
config          = AcabConfig.Get()

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
        assert all([isinstance(x, Sentence) for x in self.structure])

        self.data[TYPE_INSTANCE_S] = TYPE_DEFINITION

        self.unify_structure_variables()

    @property
    def structure(self):
        return self.value

    def __eq__(self, other):
        path_eq = self.path == other.path
        structure_len = len(self.structure) == len(other.structure)
        structure_eq = all([x == y for x,y in zip(self.structure, other.structure)])

        return path_eq and structure_len and structure_eq


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
        #     prefix = Sentence.build(sen.words[:-1] + [sen.words[-1].to_word()])
        #     flat_structure.append(prefix)
        #     flat_structure += [Sentence.build(prefix.words + x.words) for x in sen[-1].structure]

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
