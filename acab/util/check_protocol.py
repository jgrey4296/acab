#!/usr/bin/env python3
"""

"""

##-- builtin imports
from __future__ import annotations

# import abc
import datetime
import enum
import functools as ftz
import itertools as itz
import logging as logmod
import pathlib as pl
import re
import time
import types
import weakref
# from copy import deepcopy
# from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable, Generator)
from uuid import UUID, uuid1

##-- end builtin imports

##-- logging
logging = logmod.getLogger(__name__)
##-- end logging

def check_protocol(cls):
    """ Decorator. Check the class implements all its methods / has no abstractmethods """

    abstracts = [x for x in dir(cls) if hasattr(getattr(cls, x), "__isabstractmethod__") and getattr(cls, x).__isabstractmethod__]
    if bool(abstracts):
        raise NotImplementedError(f"Class has Abstract Methods: {cls.__module__} : {cls.__name__} : {abstracts}")

    return cls
