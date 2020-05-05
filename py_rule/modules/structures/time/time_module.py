from py_rule.abstract.module_interface import ModuleInterface
from .parsing import parser as TP


class TimeSpec(ModuleInterface):
    """ TimeParser Class, providing entry points
    for an engine and working memory to parse Time Strings
    """

    def __init__(self):
        # TODO make a time statement as well
        super().__init__()

    def parse_string(self, s):
        return TP.parseString(s)

    def assert_parsers(self, pt):
        pt.add("values.time", TP.main_pattern)

    def query_parsers(self, pt):
        TP.HOTLOAD_VALUE << pt.query("valbind")
        TP.HOTLOAD_BIND << pt.query("valbind")
