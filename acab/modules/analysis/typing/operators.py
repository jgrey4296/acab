""" Operators for manipulating types

Sum type pattern match?
Sum type combination

"""
from acab import types as AT
from acab.core.value.instruction import ActionOperator, ProductionOperator
from acab.core.util.decorators.semantic import OperatorSugar
from .values.acab_type import TypeStatement
from acab.modules.analysis.typing.unify.util import gen_f
from acab.modules.analysis.typing.unify import type_unify_fns as tuf
from acab.interfaces.value import ValueFactory as VF

Value      = AT.Value
Sentence_A = AT.Sentence
CtxIns     = AT.CtxIns
Node       = AT.Node

class TypeApply(ActionOperator):
    """
    Update a Ctx with new type assignments
    """
    def __call__(self, *args):
        pass


class TypeUnion(ProductionOperator):
    """
    Unify two data structures to get a substitution list
    """
    def __call__(self, *args):
        pass

