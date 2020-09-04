from acab.abstract.engine.dsl_fragment import DSL_Fragment

from .query_operators import EQ, NEQ, RegMatch, ELEM, HasTag
from . import query_op_parsers as QOP

class MODULE(DSL_Fragment):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()

    def assert_parsers(self, pt):
        pt.add("query.annotation.hastag", QOP.tagList)
        #        "operator.query.eq", QO.EQ,
        #        "operator.query.neq", QO.NEQ,
        #        "operator.query.regmatch", QO.RegMatch,
        #        "operator.query.elem", QO.ELEM,
        #        "operator.query.hastag", QO.HasTag,
        pass


    def query_parsers(self, pt):
        pass

    def init_strings(self):
        return []
