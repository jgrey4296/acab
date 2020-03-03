from py_rule.abstract.mod_interface import ModuleSpecification
from .parsing import parser as TP


class TimeSpec(ModuleSpecification):
    """ TimeParser Class, providing entry points
    for an engine and working memory to parse Time Strings
    """

    def __init__(self):
        super().__init__(types=[], funcs=[])
        self._value_parsers = [TP.main_pattern]

    def parse_string(self, s):
        return TP.parseString(s)

    def construct_operators(self):
        return