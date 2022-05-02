#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

logging = root_logger.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

from acab.core.config.config import AcabConfig
from acab.core.printing.basic import PrinterFragment
from acab.core.util.part_implementations.handler_system import HandlerSpec
import acab.modules.analysis.typing.printer as TPR

from acab.core.parsing import pyparse_dsl as ppDSL

from .parsing import TypeDefParser as TDefP
from .parsing import TypeParser as TP
from .operators import UnifyTypeMatch

config = AcabConfig()

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

TYPE_INSTANCE = config.attr.Print.Signals.TYPE_INSTANCE
TYPE_DEF      = config.attr.Print.Signals.TYPE_DEF
SUM_TYPE      = config.attr.Print.Signals.SUM_TYPE
OP_DEF        = config.attr.Print.Signals.OP_DEF
TYPE_CLASS    = config.attr.Print.Signals.TYPE_CLASS

TypingDSL = DSL_Fragment(specs=[DSL_Spec("sentence", struct=TDefP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT]),
                                DSL_Spec("sentence", struct=TP.HOTLOAD_SEN, flags=[DSL_Spec.flag_e.COLLECT])],
                         handlers=[DSL_Handler("sentence.ends", func=TDefP.COMBINED_DEFS),
                                   DSL_Handler("word.annotation", func=TP.TYPEDEC_CORE)])


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
