from acab.interfaces.dsl import DSL_Spec, DSL_Handler
from acab.core.util.fragments import DSL_Fragment

from .parsing import parser as TP


time_dsl = DSL_Fragment(specs=[DSL_Spec("word.valbind", TP.HOTLOAD_VALUE),
                               DSL_Spec("word.valbind", TP.HOtLOAD_BIND)],
                        handlers=[DSL_Handler("value.time", TP.main_pattern)])
