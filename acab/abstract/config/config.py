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

from configparser import ConfigParser, ExtendedInterpolation
from dataclasses import InitVar, dataclass, field
from enum import Enum, EnumMeta
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import pyparsing as pp

from acab.error.acab_config_exception import AcabConfigException

from .modal import ModalConfig

actions_e = Enum("Config Actions", "STRIPQUOTE KEYWORD LITERAL DICT LIST UNESCAPE SPLIT PSEUDOSEN")

def GET(*args, hooks=None):
    config = AcabConfig.Get(*args, hooks=hooks)
    return config

DEFAULT_ACTIONS = {actions_e.STRIPQUOTE : lambda x: x.strip("\"'"),
                   actions_e.KEYWORD    : lambda x: pp.Keyword(x),
                   actions_e.LITERAL    : lambda x: pp.Literal(x),
                   actions_e.LIST       : lambda x: x.split("\n"),
                   actions_e.UNESCAPE   : lambda x: x.encode().decode("unicode_escape"),
                   actions_e.SPLIT      : lambda x: x.split(" "),
                   actions_e.PSEUDOSEN  : lambda x: f"_:{x}"
                   }

@dataclass
class ConfigSpec():

    section : str             = field()
    key     : str             = field(default=None)
    actions : List[actions_e] = field(default_factory=list)
    as_list : bool            = field(default=False)
    as_dict : bool            = field(default=False)

    def __call__(self):
        inst = AcabConfig.Get()
        return inst(self)

@dataclass
class AcabConfig():
    """ A Singleton class for the active configuration
    Uses ${SectionName:Key} interpolation in values,
    Turns multi-line values into lists
    """
    paths     : InitVar[List[str]] = field()
    hooks     : Set[Callable]      = field(default_factory=set)

    _files    : List[str]              = field(init=False, default_factory=set)
    _config   : ConfigParser           = field(init=False)

    # Populated by hooks:
    enums              : Dict[str, EnumMeta] = field(init=False, default_factory=dict)
    defaults           : Dict[str, Enum]     = field(init=False, default_factory=dict)
    syntax_extension   : Dict[str, Enum]     = field(init=False, default_factory=dict)
    printing_extension : Dict[Enum, str]     = field(init=False, default_factory=dict)

    actions   : Dict[Any, Callable]    = field(init=False, default_factory=lambda: DEFAULT_ACTIONS)
    instance  : ClassVar['AcabConfig'] = field(init=False, default=None)
    actions_e : Enum                   = field(init=False, default=actions_e)

    @staticmethod
    def Get(*paths: List[str], hooks=None):
        _hooks = set()
        if paths is None:
            paths = []
        if hooks is not None and isinstance(hooks, list):
            _hooks.update(hooks)
        elif hooks is not None and isisntance(hooks, callable):
            _hooks.add(hooks)


        if AcabConfig.instance is None:
            AcabConfig(paths, _hooks)
        else:
            AcabConfig.instance.hooks.update(_hooks)
            AcabConfig.instance.read(paths)

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
        return key in self._config

    def value(self, section, key=None, actions=None, as_list=None, as_dict=None):
        """
        Get a lookup tuple at run time
        """
        in_file = section in self._config

        if in_file and as_list and key is None:
            return list(self._config[section].keys())
        elif in_file and as_dict and key is None:
            return dict(self._config[section].items())
        elif not in_file:
            raise AcabConfigException(f"missing util value: {section} {key}")

        # try:
        value = None
        in_section = key is None or key in self._config[section]

        if in_section:
            value = self._config[section][key]
        else:
            raise AcabConfigException(f"missing util value: {section} {key}")

        if value is None:
            value = key

        if actions is None:
            actions = []

        for action in actions:
            value = self.actions[action](value)

        return value



    def prepare(self, section, key=None, actions=None, as_list=False, as_dict=False):
        spec = ConfigSpec(section, key, actions, as_list, as_dict)
        return self.check(spec)

    def check(self, spec: ConfigSpec):
        """
        Returns an accessor tuple for later,
        while guaranteeing the section:key:value does exist

        This lets AcabPrintSemantics be a proxy with AcabPrintSemantics.value
        """
        assert(isinstance(spec, AcabConfigSpec))
        in_file = section in self._config
        in_section = in_file and (key is None or key in self._config[section])

        if in_section:
            return spec

        raise AcabConfigException("missing util value: {} {}".format(section, key))

    @property
    def loaded(self):
        return bool(self._files)

    def read(self, paths: List[str]):
        full_paths = []
        # DFS over provided paths, finding .config files:
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
