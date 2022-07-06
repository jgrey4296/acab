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
from pathlib import Path
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Dict, Generic,
                    Iterable, Iterator, List, Mapping, Match, MutableMapping,
                    Optional, Protocol, Sequence, Set, Tuple, Type, TypeAlias,
                    TypeVar, Union, cast)

from acab.error.config import AcabConfigException
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
    _instance : ClassVar[Config_i] = None

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(ConfigSingletonMeta, cls).__init__(name, bases, data) #type:ignore


    def __call__(cls, *paths:str|Path, hooks:None|bool|list[Callable[..., Any]]=None, **kwargs) -> None|Config_i:
        """
        If config Exists, then update it's paths and hooks, otherwise build
        If hooks is `False`, the hooks of the config will be cleared
        """

        paths  = paths or tuple()
        _hooks = set(hooks or [])

        match ConfigSingletonMeta._instance, paths, _hooks:
            case None, _, _ if 'build' in kwargs:
                logging.info("Building {}.{} Singleton", cls.__module__, cls.__qualname__)
                ConfigSingletonMeta._instance : Config_i = super().__call__(paths, _hooks)
            case None, _, _:
                raise AcabConfigException("Trying to get an AcabConfig before it has been built, call AcabConfig(..., build=True) before importing anything else")
            case _, _, _ if 'build' in kwargs:
                logging.warning("Unnecessary Call to build AcabConfig")
            case _, [], []:
                pass
            case _, _, _:
                logging.info("Updating Hooks and Read List of {}.{}", cls.__module__, cls.__qualname__) #type:ignore
                ConfigSingletonMeta._instance.hooks.update(_hooks) #type:ignore
                ConfigSingletonMeta._instance.read(list(paths)) #type:ignore

        if isinstance(hooks, bool) and not hooks:
            ConfigSingletonMeta._instance.hooks.clear()

        return ConfigSingletonMeta._instance
