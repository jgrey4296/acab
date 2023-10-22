#!/usr/bin/env python3
##-- imports
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

from acab.interfaces import fragments as FI
from acab.interfaces import handler_system as HS
from acab_config import AcabProtocolError as APE

logging = root_logger.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

##-- end imports

@APE.assert_concrete
class HandlerFragment(FI.HandlerFragment_i):

    def __bool__(self):
        return bool(self.handlers) or bool(self.specs)

    def __len__(self):
        return len(self.handlers) + len(self.specs)

    def __repr__(self):
        return f"<Handler Fragment for {self.target_i}: {len(self.specs)} Specs, {len(self.handlers)} Handlers>"

    def __iter__(self):
        for x in self.specs:
            yield x

        for y in self.handlers:
            yield y

    def __contains__(self, other):
        if isinstance(other, HS.HandlerSpec_i):
            return other in self.specs
        elif isinstance(other, HS.Handler_i):
            return other in self.handlers
        else:
            return False

class DSL_Fragment(HandlerFragment, FI.DSL_Fragment_i):
    pass

class Semantic_Fragment(HandlerFragment, FI.Semantic_Fragment_i):
    pass

class PrinterFragment(HandlerFragment, FI.Printer_Fragment_i):
    pass
