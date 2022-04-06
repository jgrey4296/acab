"""
The Typing Submodule Provides a TypeChecker,

"""
from acab.core.config.config import AcabConfig

if AcabConfig._instance is not None:
    from acab.interfaces.printing import Printer_Fragment
    from acab.core.util.part_implementations.handler_system import HandlerSpec
    import acab.modules.analysis.typing.printer as TPR
    from acab.modules.analysis.typing.dsl import TypingDSL
    print_fragment = Printer_Fragment(specs=[HandlerSpec("TYPE_INSTANCE"),
                                             HandlerSpec("TYPE_DEF"),
                                             HandlerSpec("SUM_TYPE"),
                                             HandlerSpec("OP_DEF"),
                                             HandlerSpec("TYPE_CLASS")],
                                      handlers=[TPR.TypeAwareValuePrinter,
                                                TPR.TypeRecordPrinter,
                                                TPR.SumTypePrinter,
                                                TPR.OperatorTypePrinter,
                                                TPR.TypeClassPrinter],
                                      sieve_fns=[])
