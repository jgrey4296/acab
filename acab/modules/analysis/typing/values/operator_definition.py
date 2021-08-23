"""
Class for defining operators
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import AcabConfig
from acab.modules.analysis.typing import util as TU

from .type_definition import TYPE_INSTANCE_S, TypeDefinition

config = AcabConfig.Get()

class OperatorDefinition(TypeDefinition):
    """ Defines the type signature of an operator"""

    def __init__(self, structure, params=None, sugar_syntax=None):
        """ The name of an operator and its type signature,
        with the binding to a ProductionOperator that is
        syntax sugared, and its inline place"""
        # eg: operator.+.$x(::num).$y(::num).$z(::num).num_plus
        if not isinstance(structure, list):
            structure = [structure]
        super().__init__(structure, params=params, data={TYPE_INSTANCE_S: TU.OPERATOR_DEFINITION})
        self._func_name = sugar_syntax

    def __hash__(self):
        return hash(str(self))
