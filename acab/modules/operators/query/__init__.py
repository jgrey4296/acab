from acab.core.parsing import pyparse_dsl as ppDSL

from . import query_op_parsers as QOP
from .query_operators import ELEM, EQ, NEQ, HasTag, RegMatch, SimpleTypeMatch

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

QueryDSL = DSL_Fragment(specs=[],
                        handlers=[DSL_Handler("word.annotation", QOP.tagList)])
#        "operator.query.eq", QO.EQ,
#        "operator.query.neq", QO.NEQ,
#        "operator.query.regmatch", QO.RegMatch,
#        "operator.query.elem", QO.ELEM,
#        "operator.query.hastag", QO.HasTag,
