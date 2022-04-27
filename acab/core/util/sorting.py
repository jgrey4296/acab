#!/usr/bin/env python3
"""
A Utility to decorate objects with a value,
and sort by that value.

This is intended for config hooks.
"""
from __future__ import annotations

import abc
from dataclasses import InitVar, dataclass, field
from functools import wraps
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

def priority(val:int) -> Callable[..., Any]:
    """
    Decorator.
    Try to add an __acab_sort_priority value to an object
    """

    def wrapper(obj:Any) -> Any:
        try:
            setattr(obj, "__acab_sort_priority", val)
        finally:
            return obj

    return wrapper

def sort_by_priority(vals:Iterable[Any]) -> Iterable[Any]:
    """
    Sort an iterable by its values' __acab_sort_priority.
    If values are missing it, default to 1000
    """
    return sorted(vals, key=lambda x: getattr(x, "__acab_sort_priority", 1000))
