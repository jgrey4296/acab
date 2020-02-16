from py_rule.abstract.mod_interface import ModuleSpecification
from .causal_do import DoOperator
from . import comparisons as C
from .delta_adjust import DeltaOperator
from .interleave import InterleaveOperator
from . import perform as P
from .set_ops import SetOperator
from . import transforms as T


class OperatorSpec(ModuleSpecification):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def construct_operators(self):
        # call operator constructors here
        DoOperator()
        DeltaOperator()
        InterleaveOperator()
        SetOperator()

        C.EQ()
        C.GT()
        C.LT()
        C.NEQ()
        C.RegMatch()
        C.ELEM()

        P.ActionAdd()
        P.ActionRetract()
        P.ActionPrint()
        P.ActionCustom()

        T.AddOp()
        T.SubOp()
        T.MulOp()
        T.DivOp()
        T.RandOp()
        T.RemainOp()
        T.RoundOp()
        T.NetOp()
        T.RegexOp()
        T.FormatOp()
