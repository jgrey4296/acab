from acab.abstract.module_interface import ModuleInterface

from . import action_operators as A

class MODULE(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        pt.add("operator.action.add", A.ActionAdd,
               "operator.action.print", A.ActionPrint)

    def query_parsers(self, pt):
        pass
    def init_strings(self):
        return []
