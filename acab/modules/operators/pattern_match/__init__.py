from acab.core.parsing import pyparse_dsl as ppDSL
from .pattern_match_op import PatternMatchOp
from . import pattern_match_parser as PMP


DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

parse_fragment = DSL_Fragment(specs=[DSL_Spec("word.valbind", struct=PMP.HOTLOAD_VALBIND),
                                     DSL_Spec("word.valbind", struct=PMP.HOTLOAD_VAR),
                                     DSL_Spec("sentence",     struct=PMP.HOTLOAD_SEN),
                                     DSL_Spec("sentence",     struct=PMP.HOTLOAD_QUERY)],
                              handlers=[DSL_Handler("transform.statement.pattern_match", PMP.parse_point)])
