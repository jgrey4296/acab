from py_rule.abstract.module_interface import ModuleInterface

from . import transform_operators as T


class MODULE(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        pt.add("operator.transform.ternary.regex", T.RegexOp,
               "operator.transform.unary.format", T.FormatOp)

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
