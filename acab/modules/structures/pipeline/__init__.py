from acab.abstract.interfaces.dsl_interface import DSL_Fragment_i
from . import AgendaParser as AP
from . import LayerParser as LP
from . import PipelineParser as PP


class AgendaMODULE(DSL_Fragment_i):
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


class LayerMODULE(DSL_Fragment_i):
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





class PipelineMODULE(DSL_Fragment_i):
    """ The Module Spec for base structures
    ie: Agenda/Layer/Pipeline
    """

    def __init__(self):
        super().__init__()

    def init_strings(self):
        return []

    def assert_parsers(self, pt):
        pt.add("agenda.body", AP.agenda_body,
               "statement.agenda", AP.agenda_stmt)

    def query_parsers(self, pt):
        basic_sen = pt.query("sentence.basic")
        query = pt.query("query.body")
        transform = pt.query("transform.body")
        action = pt.query("action.body")

        AP.HOTLOAD_BASIC_SEN << basic_sen
        AP.HOTLOAD_QUERY << query
        AP.HOTLOAD_TRANSFORM << transform
        AP.HOTLOAD_ACTION << action
