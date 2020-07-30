from acab.abstract.dsl_fragment import DSL_Fragment

from .layer_actions import LayerRunAgenda, LayerRunRules, LayerPerform

class MODULE(DSL_Fragment):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        # pt.add("operator.transform.n_ary.run_agenda", LA.LayerRunAgenda,
        #        "operator.transform.n_ary.run_rules", LA.LayerRunRules,
        #        "operator.action.layer_perform", LA.LayerPerform)
        pass

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
