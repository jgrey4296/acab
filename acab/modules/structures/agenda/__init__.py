from acab.abstract.engine.dsl_fragment import DSL_Fragment

from . import AgendaParser as AP


class MODULE(DSL_Fragment):
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
