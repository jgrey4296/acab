from acab.abstract.engine.dsl_fragment import DSL_Fragment

from . import PipelineParser as PP


class MODULE(DSL_Fragment):
    """ The Module Spec for base structures
    ie: Agenda/Layer/Pipeline
    """

    def __init__(self):
        super().__init__()

    def init_strings(self):
        return []

    def assert_parsers(self, pt):
        pt.add("pipeline.body", PP.pipeline_body,
               "statment.pipeline", PP.pipeline_stmt)

    def query_parsers(self, pt):
        basic_sen = pt.query("sentence.basic")
        query = pt.query("query.body")
        transform = pt.query("transform.body")
        action = pt.query("action.body")

        PP.HOTLOAD_BASIC_SEN << basic_sen
        PP.HOTLOAD_QUERY << query
        PP.HOTLOAD_TRANSFORM << transform
        PP.HOTLOAD_ACTION << action
