from py_rule.abstract.mod_interface import ModuleSpecification
from .parsing import NumberParser as NP
from . import comparison_operators as CO
from . import transform_operators as TO

class NumberSpecification(ModuleSpecification):
    """ A Module that provides numbers """

    def __init__(self):
        super(NumberSpecification, self).__init__()
        self._value_parsers = [NP.NUM]

    def parse_string(self, s):
        """ Takes a String, parses it into Data format """
        return NP.parse_string(s)

    def construct_operators(self):
        """ Use this to call operator constructors """
        CO.GT()
        CO.LT()
        TO.AddOp()
        TO.SubOp()
        TO.DivOp()
        TO.MulOp()
        TO.RandOp()
        TO.RemainOp()
        TO.RoundOp()
        TO.NegOp()

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        return []
