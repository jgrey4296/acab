from acab.interfaces.dsl import DSL_Fragment, DSL_Spec, DSL_Handler

from .parsing import TypeDefParser as TDP
from .parsing import TypeParser as TP


TypingDSL = DSL_Fragment(specs=[DSL_Spec("sentence", struct=TDP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT]),
                                DSL_Spec("sentence", struct=TP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT])],
                         handlers=[DSL_Handler("sentence.ends", TDP.COMBINED_DEFS),
                                   DSL_Handler("word.annotation", TP.TYPEDEC_CORE)])
