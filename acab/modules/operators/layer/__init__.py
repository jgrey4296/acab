from acab.abstract.interfaces.dsl_interface import DSL_Interface

from .layer_actions import LayerRunAgenda, LayerRunRules, LayerPerform

class MODULE(DSL_Interface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        # pt.add("operator.transform.run_agenda", LA.LayerRunAgenda,
        #        "operator.transform.run_rules", LA.LayerRunRules,
        #        "operator.action.layer_perform", LA.LayerPerform)
        pass

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
