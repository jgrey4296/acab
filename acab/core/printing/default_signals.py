#!/usr/bin/env python3
from __future__ import annotations

import abc
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

from acab.core.config.config import AcabConfig

config = AcabConfig()


ATOM               = config.prepare("Print.Signals", "ATOM")()
ANNOTATIONS        = config.prepare("Print.Signals", "ANNOTATIONS")()
SENTENCE           = config.prepare("Print.Signals", "SENTENCE")()
SYMBOL             = config.prepare("Print.Signals", "SYMBOL")()
CONSTRAINT         = config.prepare("Print.Signals", "CONSTRAINT")()
CONTAINER          = config.prepare("Print.Signals", "CONTAINER")()
IMPLICIT_CONTAINER = config.prepare("Print.Signals", "IMPLICIT_CONTAINER")()
MODAL              = config.prepare("Print.Signals", "MODAL")()
COMPONENT          = config.prepare("Print.Signals", "COMPONENT")()
TYPE_INSTANCE      = config.prepare("Print.Signals", "TYPE_INSTANCE")()
STRUCTURE          = config.prepare("Print.Signals", "STRUCTURE")()
TAGS               = config.prepare("Print.Signals", "TAGS")()
