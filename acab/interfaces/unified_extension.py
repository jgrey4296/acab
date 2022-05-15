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

from acab.interfaces import fragments as FI

@runtime_checkable
class UnifiedExtension_p(Protocol):

    def build_dsl(self) -> FI.DSL_Fragment_i: pass
    def build_printer(self) -> FI.Printer_Fragment_i: pass
    def build_semantics(self) -> FI.Semantic_Fragment_i: pass
