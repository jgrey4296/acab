from py_rule.abstract.module_interface import ModuleInterface
from .parsing import parser as TP


class TimeSpec(ModuleInterface):
    """ TimeParser Class, providing entry points
    for an engine and working memory to parse Time Strings
    """

    def __init__(self):
        super().__init__(value_ps=[TP.main_pattern])

    def parse_string(self, s):
        return TP.parseString(s)

    def construct_operators(self):
        return
