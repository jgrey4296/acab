#!/usr/bin/env python3
"""
Standardized Actions for use in Config.prepare

"""
import importlib
from enum import Enum, auto
from types import ModuleType
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

from acab import types as AT
from acab.core.util.decorators.util import mapToEnum


class ConfigActions(Enum):
    """
    Enum class for actions which can be mapped and used in config value preparation
    """
    STRIPQUOTE = auto()
    KEYWORD    = auto()
    LITERAL    = auto()
    DICT       = auto()
    LIST       = auto()
    UNESCAPE   = auto()
    SPLIT      = auto()
    PSEUDOSEN  = auto()
    BOOL       = auto()
    IMPORT     = auto()


Action_f        : TypeAlias            = Callable[[str], Any]
DEFAULT_ACTIONS : dict[Enum, Action_f] = {}

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.STRIPQUOTE)
def stripquote(x:str) -> str:
    return x.strip("\"'")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.LIST)
def split_lines(x:str) -> list[str]:
    return x.split("\n")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.UNESCAPE)
def unescape(x:str) -> str:
    return x.encode().decode("unicode_escape")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.SPLIT)
def split(x:str) -> list[str]:
    return x.split(" ")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.PSEUDOSEN)
def pseudosen(x:str) -> str:
    return f"_:{x}"

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.BOOL)
def is_bool(x:str) -> bool:
    return x == "True"

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.IMPORT)
def import_mod(x:str) -> ModuleType:
    return importlib.import_module(x)
