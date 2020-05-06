from py_rule.abstract.module_interface import ModuleInterface

from . import agenda_actions as AA


class MODULE(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        pt.add("operator.transform.statement.agenda_sort", AA.AgendaSort,
               "operator.transform.statement.agenda_select", AA.AgendaSelect,
               "operator.action.agenda_return", AA.AgendaReturn)

    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
