from __future__ import annotations

import importlib
import logging as logmod
import warnings
from collections import defaultdict
from configparser import ConfigParser, ExtendedInterpolation
from dataclasses import InitVar, dataclass, field
from enum import Enum, EnumMeta
from os import listdir
from os.path import (abspath, commonpath, exists, expanduser, isdir, isfile,
                     join, split, splitext)
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Dict, Generic,
                    Iterable, Iterator, List, Mapping, Match, MutableMapping,
                    Optional, Protocol, Sequence, Set, Tuple, Type, TypeAlias,
                    TypeVar, Union, cast)

from acab.interfaces.config import Config_i, ConfigSpec_d

if TYPE_CHECKING:
    # tc only imports
    from acab import types as AT
    GenFunc : TypeAlias = AT.fns.GenFunc

logging = logmod.getLogger(__name__)
override_constructor : Callable[..., defaultdict[str,Any]] = lambda: defaultdict(lambda: {})

class ConfigSingletonMeta(type(Protocol)):
    """
    A Subclass of Protocol's meta class,
    so singletons can be explicit protocol implementers
    """
    _instance : ClassVar[Config_i]

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(ConfigSingletonMeta, cls).__init__(name, bases, data) #type:ignore


    def __call__(cls, *paths:str, hooks:None|bool|list[Callable[..., Any]]=None) -> Config_i:
        """
        If config Exists, then update it's paths and hooks, otherwise build
        """
        paths  = paths or tuple()
        _hooks = set(hooks or [])

        if not hasattr(ConfigSingletonMeta, "_instance") or ConfigSingletonMeta._instance is None:
            logging.info("Building {}.{} Singleton", cls.__module__, cls.__qualname__)
            ConfigSingletonMeta._instance : Config_i = super().__call__(paths, _hooks)
        elif bool(hooks) or bool(paths):
            logging.info("Updating Hooks and Read List of {}.{}", cls.__module__, cls.__qualname__) #type:ignore
            ConfigSingletonMeta._instance.hooks.update(_hooks) #type:ignore
            ConfigSingletonMeta._instance.read(list(paths)) #type:ignore

        if isinstance(hooks, bool) and not hooks:
            ConfigSingletonMeta._instance.hooks.clear()

        return ConfigSingletonMeta._instance
