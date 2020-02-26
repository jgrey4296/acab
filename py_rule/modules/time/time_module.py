from py_rule.abstract.mod_interface import ModuleSpecification
from .parsing import parser as TP


class TimeSpec(ModuleSpecification):
    """ TimeParser Class, providing entry points
    for an engine and working memory to parse Time Strings
    """

    def __init__(self):
        super().__init__(parser=TP.main_pattern,
                         types=[],
                         funcs=[])

    def parse_string(self, s):
        return TP.parseString(s)

    def construct_operators(self):
        return
