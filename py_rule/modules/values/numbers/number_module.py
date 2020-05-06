from py_rule.abstract.module_interface import ModuleInterface
from .parsing import NumberParser as NP
from . import query_operators as CO
from . import transform_operators as TO

class NumberSpecification(ModuleInterface):
    """ A Module that provides numbers """

    def __init__(self):
        super(NumberSpecification, self).__init__()

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
        pt.add("values.numbers", NP.NUM)

        pt.add("operators.query.gt", CO.GT,
               "operators.query.lt", CO.LT)

        pt.add("operators.transform.binary.add", TO.AddOp,
               "operators.transform.binary.sub", TO.SubOp,
               "operators.transform.binary.mul", TO.MulOp,
               "operators.transform.binary.div", TO.DivOp,
               "operators.transform.binary.rand", TO.RandOp,
               "operators.transform.binary.remain", TO.RemainOp,
               "operators.transform.unary.round", TO.RoundOp,
               "operators.transform.unary.neg", TO.NegOp)
