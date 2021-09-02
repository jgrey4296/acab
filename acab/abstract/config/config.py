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
import logging as root_logger
from configparser import ConfigParser, ExtendedInterpolation
from dataclasses import InitVar, dataclass, field
from enum import Enum, EnumMeta
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from collections import defaultdict
import importlib

import pyparsing as pp
from acab.error.acab_config_exception import AcabConfigException

logging = root_logger.getLogger(__name__)

actions_e = Enum("Config Actions", "STRIPQUOTE KEYWORD LITERAL DICT LIST UNESCAPE SPLIT PSEUDOSEN BOOL IMPORT")

DEFAULT_ACTIONS = {actions_e.STRIPQUOTE : lambda x: x.strip("\"'"),
                   actions_e.LIST       : lambda x: x.split("\n"),
                   actions_e.UNESCAPE   : lambda x: x.encode().decode("unicode_escape"),
                   actions_e.SPLIT      : lambda x: x.split(" "),
                   actions_e.PSEUDOSEN  : lambda x: "_:{}".format(x),
                   actions_e.BOOL       : lambda x: True if x == "True" else False,
                   actions_e.IMPORT     : lambda x: importlib.import_module(x)}

override_constructor = lambda: defaultdict(lambda: {})
#--------------------------------------------------
def GET(*args, hooks=None):
    config = AcabConfig.Get(*args, hooks=hooks)
    return config
#--------------------------------------------------
@dataclass
class ConfigSpec():
    """ Dataclass to describe a config file value,
    and any transforms it needs prior to use """

    section : str             = field()
    key     : Optional[str]   = field(default=None)
    actions : List[actions_e] = field(default_factory=list)
    as_list : bool            = field(default=False)
    as_dict : bool            = field(default=False)
    as_enum : bool            = field(default=False)

    def __call__(self):
        inst = AcabConfig.Get()
        return inst(self)

    def __hash__(self):
        return hash(f"{self.section}:{self.key}")

#--------------------------------------------------
# pylint: disable=too-many-instance-attributes
@dataclass
class AcabConfig():
    """ A Singleton class for the active configuration
    Uses ${SectionName:Key} interpolation in values,
    Turns multi-line values into lists
    """
    paths     : InitVar[List[str]] = field()
    hooks     : Set[Callable]      = field(default_factory=set)

    _files    : Set[str]              = field(init=False, default_factory=set)
    _config   : ConfigParser          = field(init=False)
    _overrides: Dict[str, str]        = field(init=False, default_factory=override_constructor)

    # Populated by hooks:
    enums              : Dict[str, EnumMeta] = field(init=False, default_factory=dict)
    defaults           : Dict[str, Enum]     = field(init=False, default_factory=dict)
    syntax_extension   : Dict[str, Enum]     = field(init=False, default_factory=dict)
    printing_extension : Dict[Enum, str]     = field(init=False, default_factory=dict)

    actions   : Dict[Any, Callable]    = field(init=False, default_factory=lambda: DEFAULT_ACTIONS)
    instance  : ClassVar['AcabConfig'] = field(init=False, default=None)
    actions_e : Enum                   = field(init=False, default=actions_e)

    @staticmethod
    def Get(*paths: str, hooks=None):
        """ Get the AcabConfig Singleton. optionally load paths of config files """
        _hooks = set()
        if paths is None:
            paths = []
        if hooks is not None and isinstance(hooks, list):
            _hooks.update(hooks)
        elif hooks is not None and isinstance(hooks, callable):
            _hooks.add(hooks)


        if AcabConfig.instance is None:
            logging.info("Building AcabConfig Singleton")
            AcabConfig(paths, _hooks)
        else:
            AcabConfig.instance.hooks.update(_hooks)
            AcabConfig.instance.read(list(paths))

        return AcabConfig.instance

    def __post_init__(self, paths: List[str]):
        if AcabConfig.instance is not None:
            raise AcabConfigException("AcabConfig Already Exists")

        AcabConfig.instance = self
        self._config = ConfigParser(interpolation=ExtendedInterpolation(), allow_no_value=True)
        self.read(paths)

    def __call__(self, lookup):
        return self.value(lookup)

    def __contains__(self, key):
        in_print = key in self.printing_extension
        in_base = key in self._config
        in_enums = key in self.enums
        in_defaults = key in self.defaults
        return any([in_print, in_base, in_enums, in_defaults])

    def value(self, val: Union[Enum, ConfigSpec]):
        """ Unified value retrieval """
        if isinstance(val, Enum):
            return self.enum_value(val)

        assert(isinstance(val, ConfigSpec))
        spec    = val

        if spec.as_enum and spec.section in self.enums:
            return self.enums[spec.section]

        return self.spec_value(spec)


    def enum_value(self, val):
        """ Lookup the print representation of an enum value """
        if val not in self.printing_extension:
            raise AcabConfigException("Unrecognised Enum request: {val}")

        return self.printing_extension[val]


    def spec_value(self, spec: ConfigSpec):
        """
        Get a lookup tuple at run time
        """
        in_file = spec.section in self._config

        if in_file and spec.as_list and spec.key is None:
            return list(self._config[spec.section].keys())
        elif in_file and spec.as_dict and spec.key is None:
            return dict(self._config[spec.section].items())
        elif not in_file:
            raise AcabConfigException(f"missing util value: {spec.section} {spec.key}")

        # try:
        value = None
        in_override= spec.key is None or spec.key in self._overrides[spec.section]
        in_section = spec.key is None or spec.key in self._config[spec.section]

        if in_override:
            value = self._overrides[spec.section][spec.key]
        elif in_section:
            value = self._config[spec.section][spec.key]
        elif not in_section:
            raise AcabConfigException(f"missing util value: {section} {key}")

        if value is None:
            value = spec.key

        actions = spec.actions or []

        for action in spec.actions:
            value = self.actions[action](value)

        return value



    def prepare(self, *args, **kwargs):
        spec = ConfigSpec(*args, **kwargs)
        return self.check(spec)
    def default(self, entry):
        """ Get the default value for an enum entry """
        if entry not in self.defaults:
            raise AcabConfigException(f"Unrecognised default request: {entry}")

        return self.defaults[entry]

    def check(self, spec: ConfigSpec):
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


    @property
    def loaded(self):
        return bool(self._files)

    def read(self, paths: List[str]):
        """ DFS over provided paths, finding .config files """
        full_paths = []

        for path in paths:
            expanded = abspath(expanduser(path))
            if isfile(expanded):
                full_paths.append(expanded)
                continue

            assert(isdir(expanded)), breakpoint()
            found = [join(expanded, x) for x in listdir(expanded)]
            full_paths += [x for x in found if splitext(x)[1] == ".config"]

        new_paths = [x for x in full_paths if x not in self._files]
        if bool(new_paths):
            self._config.read(new_paths)
            self._files.update(new_paths)
            self._run_hooks()
        return self


    def _run_hooks(self):
        for hook in self.hooks:
            hook(self)

    def override(self, spec: ConfigSpec, value):
        assert(isinstance(spec, ConfigSpec))
        section = spec.section
        key     = spec.key
        self._overrides[section][key] = value
