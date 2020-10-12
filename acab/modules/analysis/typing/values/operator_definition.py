from acab.config import AcabConfig

from acab.abstract.core.type_base import TypeInstance

from acab.modules.analysis.typing import util as TU

from .type_definition import TypeDefinition

util = AcabConfig.Get()

class OperatorDefinition(TypeDefinition):
    """ Defines the type signature of an operator"""

    def __init__(self, structure, params=None, sugar_syntax=None):
        """ The name of an operator and its type signature,
        with the binding to a ProductionOperator that is
        syntax sugared, and its inline place"""
        # eg: operator.+.$x(::num).$y(::num).$z(::num).num_plus
        if not isinstance(structure, list):
            structure = [structure]
        super().__init__(structure, params=params, _type=TU.OPERATOR_DEFINITION)
        self._func_name = sugar_syntax

    def __hash__(self):
        return hash(str(self))


    def pprint(self, opts=None):
        raise DeprecationWarning("Use Print Semantics")
        # return "{}: (::λ) {}".format(self._name,
                                     # self._structure[0].pprint())

    def pprint_body(self, val):
        raise DeprecationWarning("Use Print Semantics")
        # assert(len(self.structure) == 1)
        # body = self.structure[0].pprint()
        # return PrU._wrap_rebind(val + body, self._func_name, is_sugar=True)
