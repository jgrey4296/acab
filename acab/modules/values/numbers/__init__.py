from acab.abstract.dsl_fragment import DSL_Fragment
from .parsing import NumberParser as NP
from .query_operators import GT, LT
from .transform_operators import AddOp, SubOp, MulOp, DivOp, RandOp, RemainOp, RoundOp, NegOp


class MODULE(DSL_Fragment):
    """ A Module that provides numbers """

    def __init__(self):
        super(MODULE, self).__init__()

    def parse_string(self, s):
        """ Takes a String, parses it into Data format """
        return NP.parse_string(s)

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        return []

    def assert_parsers(self, pt):
        pt.add("value.number", NP.NEG_NUM)


