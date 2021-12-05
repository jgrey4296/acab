from acab.interfaces.dsl import DSL_Fragment, DSL_Handler

from .query_operators import EQ, NEQ, RegMatch, ELEM, HasTag, TypeMatch
from . import query_op_parsers as QOP

QueryDSL = DSL_Fragment(specs=[],
                        handlers=[DSL_Handler("word.annotation", QOP.tagList)])
#        "operator.query.eq", QO.EQ,
#        "operator.query.neq", QO.NEQ,
#        "operator.query.regmatch", QO.RegMatch,
#        "operator.query.elem", QO.ELEM,
#        "operator.query.hastag", QO.HasTag,
