from py_rule.abstract.mod_interface import ModuleSpecification
from .parsing import TypeDefParser as TDP
from .parsing import TypeParser as TP


class TypingSpec(ModuleSpecification):
    """ TimeParser Class, providing entry points
    for an engine and working memory to parse Time Strings
    """

    def __init__(self):
        super().__init__(types=[],
                         funcs=[])
        # TODO setup value parsers
        # todo setup statement parsers
        # add functions

    def parse_string(self, s):
        return TP.parseString(s)

    def construct_operators(self):
        return
