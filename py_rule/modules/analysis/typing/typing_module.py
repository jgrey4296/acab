from py_rule.abstract.module_interface import ModuleInterface
from .type_checker import TypeChecker
from .parsing import TypeDefParser as TDP
from .parsing import TypeParser as TP


class TypingSpec(ModuleInterface):
    """ Typing Spec Class, providing entry points
    for an engine and working memory to handle type inference

    Add parsers to:
    ._[value/statement/annotation]_parsers

    Types to     ._types
    Functions to ._functions

    Implement:
    parse_string,
    init_strings
    """

    def __init__(self):
        super().__init__(statement_ps=[TDP.COMBINED_DEFS],
                         annotate_ps=[TP.TYPEDEC_CORE])

    def parse_string(self, s):
        return TP.parseString(s)

    def init_strings(self):
        return ""

    def assert_parsers(self, pt):
        pt.add("statements.typing", TDP.COMBINED_DEFS)
        pt.add("annotations.typing", TP.TYPEDEC_CORE)

    def query_parsers(self, pt):
        TDP.HOTLOAD_BASIC_SEN << pt.query("sentences.basic")
        TDP.HOTLOAD_PARAM_SEN << pt.query("sentences.param")

        TP.HOTLOAD_BASIC_SEN << pt.query("sentences.basic")
