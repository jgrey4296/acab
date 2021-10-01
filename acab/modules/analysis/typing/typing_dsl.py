from acab.abstract.interfaces.dsl import DSL_Fragment_i

from .parsing import TypeDefParser as TDP
from .parsing import TypeParser as TP


class TypingDSL(DSL_Fragment_i):
    """ Typing Spec Class, providing entry points
    for an engine and working memory to handle type inference

    Add parsers to:
    ._[value/statement/annotation]_parsers

    Types to     ._types
    Functions to ._functions

    Implement:
    parse_string,
    """
    def parse_string(self, s):
        return TP.parseString(s)

    def assert_parsers(self, pt):
        pt.add("sentence.ends.statement.typing" , TDP.COMBINED_DEFS)
        pt.add("word.annotation.typing"         , TP.TYPEDEC_CORE)
        # pt.add("operator.action.typecheck", TypeChecker)

    def query_parsers(self, pt):
        TDP.HOTLOAD_SEN << pt.query("sentence")
        TP.HOTLOAD_SEN  << pt.query("sentence")
