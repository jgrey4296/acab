#!/usr/bin/env python3
##-- imports
from __future__ import annotations

import abc
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref


import pyparsing as pp
import acab
from acab.core.parsing import debug_funcs as DBF

##-- end imports

logging = logmod.getLogger(__name__)
config  = acab.config


def pytest_parse_debug_hook(b:bool):
    """
    If testing using pytest,
    activate the pyparsing debug funcs.
    Allows test failures to have useful log output for parse failures
    """
    if '@pytest_ar' in globals() and b:
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)
