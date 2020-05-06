from py_rule.abstract.module_interface import ModuleInterface

from .agenda import AgendaParser as AP
from .layer import LayerParser as LP
from .pipeline import PipelineParser as PP


class StandardStructures(ModuleInterface):
    """ The Module Spec for base structures
    ie: Agenda/Layer/Pipeline
    """

    def __init__(self):
        super().__init__()

    def init_strings(self):
        return []

    def assert_parsers(self, pt):
        pt.add("agenda.body", AP.agenda_body,
               "statements.agenda", AP.agenda_stmt)

        pt.add("layer.body", LP.layer_body,
               "statements.layer", LP.layer_stmt)

        pt.add("pipeline.body", PP.pipeline_body,
               "statments.pipeline", PP.pipeline_stmt)

    def query_parsers(self, pt):
        basic_sen = pt.query("sentences.basic")
        query = pt.query("query.body")
        transform = pt.query("transform.body")
        action = pt.query("action.body")

        AP.HOTLOAD_BASIC_SEN << basic_sen
        AP.HOTLOAD_QUERY << query
        AP.HOTLOAD_TRANSFORM << transform
        AP.HOTLOAD_ACTION << action

        LP.HOTLOAD_BASIC_SEN << basic_sen
        LP.HOTLOAD_QUERY << query
        LP.HOTLOAD_TRANSFORM << transform
        LP.HOTLOAD_ACTION << action

        PP.HOTLOAD_BASIC_SEN << basic_sen
        PP.HOTLOAD_QUERY << query
        PP.HOTLOAD_TRANSFORM << transform
        PP.HOTLOAD_ACTION << action
