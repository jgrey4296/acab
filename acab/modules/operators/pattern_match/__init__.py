from acab.interfaces.dsl import DSL_Fragment_i

from .pattern_match_op import PatternMatchOp
from . import pattern_match_parser as PMP


class MODULE(DSL_Fragment_i):
    """ The Module Spec for base operators """

    def assert_parsers(self, pt):
        pt.add("transform.statement.pattern_match", PMP.parse_point)

    def query_parsers(self, pt):
        PMP.HOTLOAD_VALBIND << pt.query("word.valbind")
        PMP.HOTLOAD_VAR     << pt.query("word.valbind")
        PMP.HOTLOAD_SEN     << pt.query("sentence")
        PMP.HOTLOAD_QUERY   << pt.query("sentence")
