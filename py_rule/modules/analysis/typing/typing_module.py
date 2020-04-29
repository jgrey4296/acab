from py_rule.abstract.mod_interface import ModuleSpecification
from .parsing import TypeDefParser as TDP
from .parsing import TypeParser as TP


class TypingSpec(ModuleSpecification):
    """ Typing Spec Class, providing entry points
    for an engine and working memory to handle type inference

    Add parsers to:
    ._[value/statement/annotation]_parsers

    Types to     ._types
    Functions to ._functions

    Implement:
    parse_string,
    construct_operators
    init_strings
    define_layers

    """

    def __init__(self):
        super().__init__(statement_ps=[TDP.COMBINED_DEFS],
                         annotate_ps=[TP.TYPEDEC_CORE])

    def parse_string(self, s):
        return TP.parseString(s)

    def construct_operators(self):
        return

    def init_strings(self):
        return ""

    def define_layers(self):
        return []
