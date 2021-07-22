from acab.abstract.interfaces.dsl_interface import DSL_Fragment_i
from acab.abstract.interfaces.semantic_interfaces import Semantic_Fragment_i

from .query_operators import EQ, NEQ, RegMatch, ELEM, HasTag
from . import query_op_parsers as QOP



class QueryDSL(DSL_Fragment_i):
    """ The Module Spec for base operators """

    def assert_parsers(self, pt):
        pt.add("annotation.hastag", QOP.tagList)
        #        "operator.query.eq", QO.EQ,
        #        "operator.query.neq", QO.NEQ,
        #        "operator.query.regmatch", QO.RegMatch,
        #        "operator.query.elem", QO.ELEM,
        #        "operator.query.hastag", QO.HasTag,
        pass


    def query_parsers(self, pt):
        pass
