from acab.abstract.interfaces.dsl_interface import DSL_Fragment_i
from .parsing import parser as TP


class MODULE(DSL_Fragment_i):
    """ TimeParser Class, providing entry points
    for an engine and working memory to parse Time Strings
    """

    def __init__(self):
        # TODO make a time statement as well
        super().__init__()

    def parse_string(self, s):
        return TP.parseString(s)

    def assert_parsers(self, pt):
        pt.add("value.time", TP.main_pattern)

    def query_parsers(self, pt):
        TP.HOTLOAD_VALUE << pt.query("valbind")
        TP.HOTLOAD_BIND << pt.query("valbind")
