from acab.core.parsing import pyparse_dsl as ppDSL

from .parsing import TypeDefParser as TDefP
from .parsing import TypeParser as TP

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

TypingDSL = DSL_Fragment(specs=[DSL_Spec("sentence", struct=TDefP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT]),
                                DSL_Spec("sentence", struct=TP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT])],
                         handlers=[DSL_Handler("sentence.ends", TDefP.COMBINED_DEFS),
                                   DSL_Handler("word.annotation", TP.TYPEDEC_CORE)])
