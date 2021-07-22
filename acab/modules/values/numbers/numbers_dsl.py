from acab.abstract.interfaces.dsl_interface import DSL_Fragment_i
from .parsing import NumberParser as NP
from .query_operators import GT, LT
from .transform_operators import AddOp, SubOp, MulOp, DivOp, RandOp, RemainOp, RoundOp, NegOp
import logging as root_logger
logging = root_logger.getLogger(__name__)


class NumbersDSL(DSL_Fragment_i):
    """ A Module that provides numbers """

    def parse_string(self, s):
        """ Takes a String, parses it into Data format """
        return NP.parseString(s)

    def assert_parsers(self, pt):
        pt.add("value.number", NP.NEG_NUM)
