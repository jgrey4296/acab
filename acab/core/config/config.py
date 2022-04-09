""" Cross-module utilities for the rule engines
Behaves as a Singleton, *must* be loaded before the rest of ACAB.

Has Two Main methods:
Prepare, and Value

Prepare ensures a config section and value exists at startup,
but defers the retrieval to later.
This enables PrintSemantics to override at print time if necessary.

Value gets the value at call time.

Actions are available for preprocessing the value

"""
from __future__ import annotations

import importlib
import logging as logmod
from collections import defaultdict
from configparser import ConfigParser, ExtendedInterpolation
from dataclasses import InitVar, dataclass, field
from enum import Enum, EnumMeta
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Protocol,
                    Sequence, Set, Tuple, Type, TypeAlias, TypeVar, Union,
                    cast)

from acab import types as AT
from acab.core.config import actions as CA
from acab.core.util.singletons import SingletonMeta
from acab.error.config import AcabConfigException
from acab.error.protocol import AcabProtocolError as APE
from acab.interfaces.config import Config_i, ConfigSpec_d
from acab.core.config.attr_gen import AttrGenerator

logging = logmod.getLogger(__name__)

GenFunc : TypeAlias = AT.fns.GenFunc
override_constructor : Callable[..., defaultdict[str,Any]] = lambda: defaultdict(lambda: {})
#--------------------------------------------------
def GET(*args:str, hooks:None|list[AT.fns.GenFunc]=None) -> Config_i:
    """
    Utility config object retriever.
    As the config object is a singleton,
    this is mainly for alternative acab system initialisation
    which doesn't use acab.setup

    Arguments:
        *args:
            paths to load
        hooks:
            Functions to extend the config object with
    Returns:
        The config object
    """
    config = AcabConfig(*args, hooks=hooks)
    return config
#--------------------------------------------------
# pylint: disable-next=too-few-public-methods
class ConfigSpec(ConfigSpec_d):
    """ Dataclass to describe a config file value,
    and any transforms it needs prior to use """

    def __call__(self) -> Any:
        config = AcabConfig() #type:ignore
        return config(self)

class ConfigSingleton(type(Protocol)):
    """
    A Subclass of Protocol's meta class,
    so singletons can be explicit protocol implementers
    """
    _instance : ClassVar[Config_i]

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(ConfigSingleton, cls).__init__(name, bases, data) #type:ignore


    def __call__(cls, *paths:str, hooks:None|list[Callable[..., Any]]=None) -> Config_i:
        """
        If config Exists, then update it's paths and hooks, otherwise build
        """
        paths  = paths or tuple()
        _hooks = set(hooks or [])

        if not hasattr(ConfigSingleton, "_instance") or ConfigSingleton._instance is None:
            logging.info(f"Building {cls.__module__}.{cls.__qualname__} Singleton")
            ConfigSingleton._instance : Config_i = super().__call__(paths, _hooks)
        elif bool(hooks) or bool(paths):
            logging.info(f"Updating Hooks and Read List of {cls.__module__}.{cls.__qualname__}") #type:ignore
            ConfigSingleton._instance.hooks.update(_hooks) #type:ignore
            ConfigSingleton._instance.read(list(paths)) #type:ignore

        return ConfigSingleton._instance



@APE.assert_implements(Config_i)
@dataclass
class AcabConfig(Config_i, metaclass=ConfigSingleton):
    """ A Singleton class for the active configuration
    Uses ${SectionName:Key} interpolation in values,
    Turns multi-line values into lists
    """
    paths     : InitVar[None|list[str]]         = field(default=None)
    hooks     : set[GenFunc]                    = field(default_factory=set)

    _files    : set[str]                        = field(init=False, default_factory=set)
    _config   : ConfigParser                    = field(init=False)
    _overrides: dict[str, str]                  = field(init=False, default_factory=override_constructor)

    # Populated by hooks:
    enums              : dict[str, EnumMeta] = field(init=False, default_factory=dict)
    defaults           : dict[str, Enum]     = field(init=False, default_factory=dict)
    syntax_extension   : dict[str, Enum]     = field(init=False, default_factory=dict)
    printing_extension : dict[Enum, str]     = field(init=False, default_factory=dict)
    attr               : AttrGenerator       = field(init=False)

    actions   : dict[Any, GenFunc]              = field(init=False, default_factory=lambda: CA.DEFAULT_ACTIONS)
    actions_e : ClassVar[Type[CA.ConfigActions]]= CA.ConfigActions


    def __post_init__(self, paths: None|list[str]):
        self._config = ConfigParser(interpolation=ExtendedInterpolation(),
                                    allow_no_value=True)
        self.attr = AttrGenerator(self._config)
        # Overrides ConfigParser's default of lowercasing everything
        self._config.optionxform = lambda x: x #type:ignore
        if bool(paths):
            self.read(paths)

        # if hasattr(self, "check_structure"):
        #     self.check_structure()
        # else:
        #     logging.warning("Config has been created without a check_structure")
        #     breakpoint()

    def __call__(self, lookup):
        return self.value(lookup) #type:ignore

    def __contains__(self, key):
        in_print    = key in self.printing_extension
        in_base     = key in self._config
        in_enums    = key in self.enums
        in_defaults = key in self.defaults
        return any([in_print, in_base, in_enums, in_defaults])

    def _run_hooks(self):
        for hook in self.hooks:
            hook(self)

    @property
    def loaded(self):
        return bool(self._files)

    def value(self, val: Enum|ConfigSpec_d):
        """ Unified value retrieval """
        if isinstance(val, Enum):
            return self._enum_value(val)

        assert(isinstance(val, ConfigSpec))
        spec    = val

        if spec.as_enum and spec.section in self.enums:
            return self.enums[spec.section]

        return self._spec_value(spec)


    def prepare(self, *args:Any, **kwargs:Any) -> ConfigSpec_d:
        """ Config utility to create a ConfigSpec for later use """
        spec = ConfigSpec(*args, **kwargs)
        return self.check(spec)

    def default(self, entry):
        """ Get the default value for an enum entry """
        if entry not in self.defaults:
            raise AcabConfigException(f"Unrecognised default request: {entry}")

        return self.defaults[entry]

    def check(self, spec: ConfigSpec_d) -> ConfigSpec_d:
        """
        Returns an accessor tuple for later,
        while guaranteeing the section:key:value does exist

        This lets AcabPrintSemantics be a proxy with AcabPrintSemantics.value
        """
        assert(isinstance(spec, ConfigSpec))
        if spec.as_enum and spec.section in self.enums:
            return spec

        in_file = spec.section in self._config
        in_section = in_file and (spec.key is None or spec.key in self._config[spec.section])

        if not in_file:
            raise AcabConfigException(f"Bad Section Specified: {spec.section}! {spec.key}")

        if not in_section:
            raise AcabConfigException(f"Bad Key Specified: {spec.section} {spec.key}!")

        return spec


    def read(self, paths: list[str]):
        """ DFS over provided paths, finding the cls.suffix filetype (default=.config) """
        full_paths = []

        for path in paths:
            expanded = abspath(expanduser(path))
            if isfile(expanded):
                full_paths.append(expanded)
                continue

            assert(isdir(expanded))
            found = [join(expanded, x) for x in listdir(expanded)]
            full_paths += [x for x in found if splitext(x)[1] == self.suffix]

        new_paths : list[str] = [x for x in full_paths if x not in self._files]
        if bool(new_paths):
            self._config.read(new_paths)
            self.attr._generate()
            self._files.update(new_paths)
            self._run_hooks()
        return self


    def override(self, spec: ConfigSpec_d, value:str) -> None:
        assert(isinstance(spec, ConfigSpec))
        section = spec.section
        key     = spec.key
        # A default dict with a dict inside:
        self._overrides[section][key] = value #type:ignore



    def _enum_value(self, val:Enum) -> str:
        """ Lookup the print representation of an enum value """
        if val not in self.printing_extension:
            raise AcabConfigException("Unrecognised Enum request: {val}")

        return self.printing_extension[val]


    #pylint: disable-next=too-many-branches
    def _spec_value(self, spec: ConfigSpec):
        """
        Get a lookup tuple at run time
        """
        in_file = spec.section in self._config
        if in_file and spec.as_list and spec.key is None:
            return list(self._config[spec.section].keys())
        elif in_file and spec.as_dict and spec.key is None:
            return dict(self._config[spec.section].items())
        elif in_file and spec.as_enum and spec.key is None:
            values = " ".join(self._config[spec.section].keys())
            # Runtime creation of enums
            self.enums[spec.section] = Enum(spec.section, values) #type:ignore
            return self.enums[spec.section]
        elif not in_file:
            raise AcabConfigException(f"missing util value: {spec.section} {spec.key}")

        # try:
        value = spec.key
        in_override= spec.key is None or spec.key in self._overrides[spec.section]
        in_section = spec.key is None or spec.key in self._config[spec.section]

        if in_override:
            # Default dict with dict inside:
            value = self._overrides[spec.section][spec.key] #type:ignore
        elif in_section and self._config[spec.section][spec.key] is not None:
            value = self._config[spec.section][spec.key] #type:ignore
        elif not in_section:
            raise AcabConfigException(f"missing util value: {spec.section} {spec.key}")


        actions = spec.actions or []
        for action in actions:
            value = self.actions[action](value)

        return value
