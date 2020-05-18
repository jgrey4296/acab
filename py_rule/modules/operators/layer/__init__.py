from py_rule.abstract.module_interface import ModuleInterface

from . import layer_actions as LA

class MODULE(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):

        pt.add("operator.transform.unary.run_agenda", LA.LayerRunAgenda,
               "operator.transform.unary.run_rules", LA.LayerRunRules,
               "operator.action.layer_perform", LA.LayerPerform)

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
