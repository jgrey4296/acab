#!/usr/bin/env python3
"""
Standardized Actions for use in Config.prepare

"""
from __future__ import annotations

import abc
import importlib
from dataclasses import InitVar, dataclass, field
from enum import Enum, auto
from types import ModuleType
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from enum import Enum, EnumMeta

if TYPE_CHECKING:
    # tc only imports
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
    IMPORT     = auto()
    IMCLASS    = auto()


Action_f        : TypeAlias            = Callable[[str], Any]
DEFAULT_ACTIONS : dict[Enum, Action_f] = {}
TYPE_ACTIONS    : dict[Type[Any], Action_f] = {}

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.STRIPQUOTE)
def stripquote(x:str, **kwargs) -> str:
    return x.strip("\"'")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.LIST)
def split_lines(x:str, **kwargs) -> list[str]:
    return x.split("\n")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.UNESCAPE)
def unescape(x:str, **kwargs) -> str:
    return x.encode().decode("unicode_escape")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.SPLIT)
def split(x:str, **kwargs) -> list[str]:
    return x.split(" ")

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.PSEUDOSEN)
def pseudosen(x:str, **kwargs) -> str:
    return f"_:{x}"

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.IMPORT)
def import_mod(x:str, **kwargs) -> ModuleType:
    return importlib.import_module(x)

@mapToEnum(DEFAULT_ACTIONS, ConfigActions.IMCLASS)
def import_class(x:str, interface:Type[Any]=type, **kwargs) -> Type[Any]:
    mod_comps = x.split(".")
    mod = importlib.import_module(".".join(mod_comps[:-1]))
    cls = getattr(mod, mod_comps[-1])
    assert(issubclass(cls, interface))
    return cls


@mapToEnum(TYPE_ACTIONS, Enum)
@mapToEnum(TYPE_ACTIONS, EnumMeta)
def to_enum(ret_section:Any, actions=None, spec=None, config=None) -> Enum:
    assert(spec.section not in config.enums)
    values = " ".join(ret_section.keys())
    # Runtime creation of enums
    config.enums[spec.section] = Enum(spec.section, values) #type:ignore
    value = config.enums[spec.section]
    if spec.key is not None:
        value = config.enums[spec.section][spec.key]
    return value

@mapToEnum(TYPE_ACTIONS, bool)
def to_bool(ret_section:Any, actions=None, spec=None, config=None) -> bool:
    return ret_section[spec.key] == "True"

@mapToEnum(TYPE_ACTIONS, list)
def to_list(ret_section:Any, actions=None, spec=None, config=None) -> list[Any]:
    assert(spec.key is None)
    value = list(ret_section.keys())
    for action in actions:
        value = [action(ret_section, **spec.args) for ret_section in value]
    return value

@mapToEnum(TYPE_ACTIONS, dict)
def to_dict(ret_section:Any, actions=None, spec=None, config=None) -> dict[Any, Any]:
    assert(spec.key is None)
    value = dict(ret_section.items())
    for action in actions:
        value = {action(k, **spec.args): action(v, **spec.args) for k,v in value.items()}

    return value

@mapToEnum(TYPE_ACTIONS, None)
def default_type(ret_section:Any, actions=None, spec=None, config=None) -> Any:
    value = ret_section[spec.key]
    if value is None:
        value = spec.key
    for action in actions:
        value = action(value, **spec.args)
    return value

@mapToEnum(TYPE_ACTIONS, False)
def default_type(ret_section:Any, actions=None, spec=None, config=None) -> Any:
    value = ret_section[spec.key]
    if value is None:
        value = spec.key

    value = spec._type(value)
    for action in actions:
        value = action(value, **spec.args)

    return value


@mapToEnum(TYPE_ACTIONS, "for_sen")
def prep_for_sentence(ret_section:Any, actions=None, spec=None, config=None) -> Any:
    assert(spec.key is not None)
    value = ret_section[spec.key]
    return value.split(" ")
