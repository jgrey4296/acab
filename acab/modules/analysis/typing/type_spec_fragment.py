#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod

logging = logmod.getLogger(__name__)

from acab import AcabConfig

config = AcabConfig()

import acab.modules.analysis.typing.printer as TPR
from acab import AcabConfig
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.util.fragments import (DSL_Fragment, PrinterFragment)
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.interfaces import handler_system as HS
from acab.interfaces.bind import Bind_i
from acab.interfaces.fragments import UnifiedFragment_p

from .parsing import TypeDefParser as TDefP
from .parsing import TypeParser as TP

Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()
Bind    = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

sentence_sig      = config.prepare("Parse.Signals", "sentence")()
sentence_ends_sig = config.prepare("Parse.Signals", "sentence.ends")()
word_ann_sig      = config.prepare("Parse.Signals", "word.annotation")()
word_sig          = config.prepare("Parse.Signals", "word.constrained")()

TYPE_INSTANCE = config.attr.Print.Signals.TYPE_INSTANCE
TYPE_DEF      = config.attr.Print.Signals.TYPE_DEF
SUM_TYPE      = config.attr.Print.Signals.SUM_TYPE
OP_DEF        = config.attr.Print.Signals.OP_DEF
TYPE_CLASS    = config.attr.Print.Signals.TYPE_CLASS

class TypeSpecFragment(UnifiedFragment_p):
    """
    An Acab Fragment for specifying types
    doesn't provide the semantics for *checking* types though
    """

    def build_dsl(self):
        TypingDSL = DSL_Fragment(specs=[DSL_Spec(sentence_sig, struct=TDefP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT]),
                                        DSL_Spec(sentence_sig, struct=TP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT]),
                                        DSL_Spec(word_sig, struct=TDefP.HOTLOAD_WORD, flags=[DSL_Spec.flag_e.COLLECT])],
                                 handlers=[DSL_Handler(sentence_ends_sig, func=TDefP.COMBINED_DEFS),
                                           DSL_Handler(word_ann_sig, func=TP.TYPEDEC_CORE)])
        return TypingDSL

    def build_printers(self):
        print_fragment = PrinterFragment(specs=[HandlerSpec(TYPE_INSTANCE),
                                                HandlerSpec(TYPE_DEF),
                                                HandlerSpec(SUM_TYPE),
                                                HandlerSpec(OP_DEF),
                                                HandlerSpec(TYPE_CLASS)],
                                         handlers=[TPR.TypeAwareValuePrinter().as_handler(),
                                                   TPR.TypeRecordPrinter().as_handler(),
                                                   TPR.SumTypePrinter().as_handler(),
                                                   TPR.OperatorTypePrinter().as_handler(),
                                                   TPR.TypeClassPrinter().as_handler()])
        return print_fragment
