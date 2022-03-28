import logging as root_logger

from acab.core.parsing import pyparse_dsl as ppDSL
from acab.interfaces.dsl import DSL_Fragment, DSL_Handler, DSL_Spec

from .parsing import NumberParser as NP
from .query_operators import GT, LT
from .transform_operators import (AddOp, DivOp, MulOp, NegOp, RandOp, RemainOp,
                                  RoundOp, SubOp)

logging = root_logger.getLogger(__name__)

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler


NumbersDSL = DSL_Fragment(specs=[],
                          handlers=[DSL_Handler("value.number", NP.NEG_NUM)])
