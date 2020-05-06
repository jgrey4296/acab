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
        pt.add("value.numbers", NP.NUM)

        pt.add("operator.query.gt", CO.GT,
               "operator.query.lt", CO.LT)

        pt.add("operator.transform.binary.add", TO.AddOp,
               "operator.transform.binary.sub", TO.SubOp,
               "operator.transform.binary.mul", TO.MulOp,
               "operator.transform.binary.div", TO.DivOp,
               "operator.transform.binary.rand", TO.RandOp,
               "operator.transform.binary.remain", TO.RemainOp,
               "operator.transform.unary.round", TO.RoundOp,
               "operator.transform.unary.neg", TO.NegOp)
