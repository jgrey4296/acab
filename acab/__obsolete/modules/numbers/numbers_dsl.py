import logging as logmod

from acab.core.parsing import pyparse_dsl as ppDSL
from acab.interfaces.dsl import DSL_Handler, DSL_Spec
from acab.core.util.fragments import DSL_Fragment

from .parsing import NumberParser as NP
from .query_operators import GT, LT
from .transform_operators import (AddOp, DivOp, MulOp, NegOp, RandOp, RemainOp,
                                  RoundOp, SubOp)

logging = logmod.getLogger(__name__)

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler


NumbersDSL = DSL_Fragment(specs=[],
                          handlers=[DSL_Handler("value.number", NP.NEG_NUM)])
