from acab.abstract.module_interface import ModuleInterface

from . import transform_operators as T


class MODULE(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        pt.add("operator.transform.n_ary.regex", T.RegexOp,
               "operator.transform.n_ary.format", T.FormatOp)

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
