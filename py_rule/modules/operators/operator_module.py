from . import comparison_operators as C
from . import action_operators as A
from . import transform_operators as T
from .causal_do import DoOperator
from .delta_adjust import DeltaOperator
from .interleave import InterleaveOperator
from .set_ops import SetOperator
from py_rule.abstract.mod_interface import ModuleSpecification

class OperatorSpec(ModuleSpecification):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def construct_operators(self):
        self._construct_misc_ops()
        self._construct_comp_ops()
        self._construct_action_ops()
        self._construct_transform_ops()

    def _construct_misc_ops(self):
        DoOperator()
        DeltaOperator()
        InterleaveOperator()
        SetOperator()

    def _construct_comp_ops(self):
        C.EQ()
        C.GT()
        C.LT()
        C.NEQ()
        C.RegMatch()
        C.ELEM()

    def _construct_action_ops(self):
        A.ActionAdd()
        A.ActionRetract()
        A.ActionPrint()
        A.ActionCustom()

    def _construct_transform_ops(self):
        T.AddOp()
        T.SubOp()
        T.MulOp()
        T.DivOp()
        T.RandOp()
        T.RemainOp()
        T.RoundOp()
        T.NegOp()
        T.RegexOp()
        T.FormatOp()
