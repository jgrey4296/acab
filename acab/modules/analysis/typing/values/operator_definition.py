from acab.util import FUNC_S, VALUE_TYPE_S, NAME_S
from acab.abstract.printing import util as PrU

from acab.modules.analysis.typing.util import OP_DEF_S, TYPE_DEF_S

from .type_definition import TypeDefinition
from .type_instance import TypeInstance


class OperatorDefinition(TypeDefinition):
    """ Defines the type signature of an operator"""

    def __init__(self, structure, params=None, sugar_syntax=None):
        """ The name of an operator and its type signature,
        with the binding to a ProductionOperator that is
        syntax sugared, and its inline place"""
        # eg: operator.+.$x(::num).$y(::num).$z(::num).num_plus
        if not isinstance(structure, list):
            structure = [structure]
        super().__init__(structure, params=params, type_str=OP_DEF_S)
        self._func_name = sugar_syntax

    def __hash__(self):
        return hash(str(self))


    def pprint_body(self, val):
        new_val = super(OperatorDefinition, self).pprint_body(val)
        return PrU._wrap_rebind(new_val, self._func_name, is_sugar=True)
