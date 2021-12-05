from acab.interfaces.dsl import DSL_Fragment, DSL_Handler, DSL_Spec
from .parsing import NumberParser as NP
from .query_operators import GT, LT
from .transform_operators import AddOp, SubOp, MulOp, DivOp, RandOp, RemainOp, RoundOp, NegOp
import logging as root_logger
logging = root_logger.getLogger(__name__)


NumbersDSL = DSL_Fragment(specs=[],
                          handlers=[DSL_Handler("value.number", NP.NEG_NUM)])
