from acab.core.parsing import pyparse_dsl as ppDSL

from acab.core.util.fragments import DSL_Fragment
from . import query_op_parsers as QOP
from .query_operators import ELEM, EQ, NEQ, HasTag, RegMatch, SimpleTypeMatch

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

QueryDSL = DSL_Fragment(specs=[],
                        handlers=[DSL_Handler("word.annotation", func=QOP.tagList)])
#        "operator.query.eq", QO.EQ,
#        "operator.query.neq", QO.NEQ,
#        "operator.query.regmatch", QO.RegMatch,
#        "operator.query.elem", QO.ELEM,
#        "operator.query.hastag", QO.HasTag,
