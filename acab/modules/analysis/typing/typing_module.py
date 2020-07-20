from acab.abstract.module_interface import ModuleInterface
from acab.abstract.printing import util as PrU
from .type_checker import TypeChecker
from .parsing import TypeDefParser as TDP
from .parsing import TypeParser as TP
from . import util as TU


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
        super().__init__()

    def parse_string(self, s):
        return TP.parseString(s)

    def init_strings(self):
        return ""

    def assert_parsers(self, pt):
        pt.add("statement.typing", TDP.COMBINED_DEFS)
        pt.add("annotation.typing", TP.TYPEDEC_CORE)
        pt.add("query.annotation.typing", TP.TYPEDEC_CORE)
        pt.add("operator.action.typecheck", TypeChecker)

    def query_parsers(self, pt):
        TDP.HOTLOAD_BASIC_SEN << pt.query("sentence.basic")
        TDP.HOTLOAD_PARAM_SEN << pt.query("sentence.param")
        TP.HOTLOAD_BASIC_SEN << pt.query("sentence.basic")
