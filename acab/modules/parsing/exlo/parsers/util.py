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

import logging as logmod
logging = logmod.getLogger(__name__)

from acab.core.parsing.annotation import ValueAnnotation, ValueRepeatAnnotation
from acab.core.parsing import default_keys as DK
from acab.core.value import default_structure as CDS

def build_flatten(s, l, t):
    value = True
    if 'sharp' in t:
        value = False

    if DK.NEGATION in t:
        value = not value

    annot = ValueAnnotation(CDS.FLATTEN, value)
    return annot
