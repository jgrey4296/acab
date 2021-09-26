"""
Classes for defining types
"""
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from dataclasses import dataclass, field, InitVar

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue, Sentence
from acab.error.acab_parse_exception import AcabParseException
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


    def set_primitive(self):
        self.set_data({PRIMITIVE_S: True})
        return self

    def build_type_instance(self, the_dict=None):
        just_path = self.path
        statement = self

        if the_dict is None:
            return Sentence.build(just_path, params=self.vars)

        new_args = []
        for x in self.vars:
            if isinstance(x, AcabValue) and x.name in the_dict:
                new_args.append(the_dict[x.name])
            else:
                assert(isinstance(x, Sentence))
                new_args.append(x)

        return Sentence.build(just_path, params=new_args)



    def unify_structure_variables(self):
        # unify shared variables across structure sentences to have the same type
        # go through all sentences
        variables = {}
        for sentence in self.structure:
            # track variables
            var_words = [x for x in sentence if x.is_var]
            missing_vars = [x.value for x in var_words if x.value not in variables]
            variables.update({x: {'types': set(), 'instances': []} for x in missing_vars})

            for word in var_words:
                variables[word.value]['instances'].append(word)
                # find variables with type annotations
                variables[word.value]['types'].add(word.type)

        # Then unify all the variables to have the same type
        for the_dict in variables.values():
            types, instances = the_dict.values()
            # TODO convert this to correct form
            # if AcabValue._type_system.BOTTOM in types:
                # types.remove(AcabValue._type_system.BOTTOM)

            if len(types) > 1:
                raise TE.TypeConflictException(types.pop(), types, self)

            if bool(types):
                type_instance = types.pop()
                for word in instances:
                    word._data[TYPE_DEF_S] = type_instance





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

    sugar_syntax : str = field(init=True, default=None)

    def __post_init__(self, sugar_syntax=None):
        """ The name of an operator and its type signature,
        with the binding to a ProductionOperator that is
        syntax sugared, and its inline place"""
        # eg: operator.+.$x(::num).$y(::num).$z(::num).num_plus
        super(OperatorDefinition, self).__post_init__()
        self._func_name = sugar_syntax

    def __hash__(self):
        return hash(str(self))

@dataclass(frozen=True)
class TypeClass(TypeDefinition):
    """ Definition of a coherent collection of functions """

    def __post_init__(self):
        super(TypeClass, self).__post_init__()
