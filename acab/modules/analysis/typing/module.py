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
