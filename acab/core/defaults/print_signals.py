#!/usr/bin/env python3
"""
These are signals the print system uses, in addition to any specific
type printers
"""
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


ATOM               = config.attr.Print.Signals.ATOM
ANNOTATIONS        = config.attr.Print.Signals.ANNOTATIONS
ANNOTATIONS_FINAL  = config.attr.Print.Signals.ANNOTATIONS_FINAL
SENTENCE           = config.attr.Print.Signals.SENTENCE
SYMBOL             = config.attr.Print.Signals.SYMBOL
CONSTRAINT         = config.attr.Print.Signals.CONSTRAINT
CONTAINER          = config.attr.Print.Signals.CONTAINER
IMPLICIT_CONTAINER = config.attr.Print.Signals.IMPLICIT_CONTAINER
MODAL              = config.attr.Print.Signals.MODAL
COMPONENT          = config.attr.Print.Signals.COMPONENT
TYPE_INSTANCE      = config.attr.Print.Signals.TYPE_INSTANCE
STRUCTURE          = config.attr.Print.Signals.STRUCTURE
TAGS               = config.attr.Print.Signals.TAGS
