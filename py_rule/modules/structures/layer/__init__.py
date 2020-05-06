from py_rule.abstract.module_interface import ModuleInterface

from . import LayerParser as LP

class MODULE(ModuleInterface):
    """ The Module Spec for base structures
    ie: Agenda/Layer/Pipeline
    """

    def __init__(self):
        super().__init__()

    def init_strings(self):
        return []

    def assert_parsers(self, pt):
        pt.add("layer.body", LP.layer_body,
               "statement.layer", LP.layer_stmt)

    def query_parsers(self, pt):
        basic_sen = pt.query("sentence.basic")
        query = pt.query("query.body")
        transform = pt.query("transform.body")
        action = pt.query("action.body")

        LP.HOTLOAD_BASIC_SEN << basic_sen
        LP.HOTLOAD_QUERY << query
        LP.HOTLOAD_TRANSFORM << transform
        LP.HOTLOAD_ACTION << action
