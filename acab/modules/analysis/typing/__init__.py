"""
The Typing Submodule Provides a TypeChecker,

"""
from acab.core.config.config import AcabConfig

if AcabConfig.instance is not None:
    from acab.interfaces.printing import Printer_Fragment
    from acab.interfaces.handler_system import HandlerSpec
    import acab.modules.analysis.typing.type_printer as TPR
    from acab.modules.analysis.typing.typing_dsl import TypingDSL
    print_fragment = Printer_Fragment(specs=[HandlerSpec("TYPE_INSTANCE"),
                                             HandlerSpec("TYPE_DEF"),
                                             HandlerSpec("SUM_TYPE"),
                                             HandlerSpec("OP_DEF"),
                                             HandlerSpec("TYPE_CLASS")],
                                      handlers=[TPR.TypeAwareValuePrinter,
                                                TPR.TypeRecordPrinter,
                                                TPR.SumTypePrinter,
                                                TPR.OperatorTypePrinter,
                                                TPR.TypeClassPrinter])
