""" Cross-module utilities for the rule engines
Behaves as a Singleton, *must* be loaded before the rest of ACAB.

Has Two Main methods:
Prepare, and Value

Prepare ensures a config section and value exists at startup,
but defers the retrieval to later.
This enables PrintSemantics to override at print time if necessary.

Value gets the value at call time.

Actions are available for preprocessing the value

TODO add hook functionality
TODO use acab errors instead
TODO have default config files
"""

from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from configparser import ConfigParser, ExtendedInterpolation
from dataclasses import dataclass, field, InitVar
from enum import Enum
import pyparsing as pp

from .modal import ModalConfig
from acab.error.acab_config_exception import AcabConfigException

actions_e = Enum("Config Actions", "STRIPQUOTE KEYWORD LITERAL DICT LIST UNESCAPE SPLIT")

def GET(*args):
    config = AcabConfig.Get(*args)
    return config

@dataclass
class AcabConfig(ModalConfig):
    """ A Singleton class for the active configuration
    Uses ${SectionName:Key} interpolation in values,
    Turns multi-line values into lists
    """
    paths : InitVar[List[str]]
    _files : List[str] = field(init=False, default_factory=set)
    _config : ConfigParser = field(init=False)
    actions_e : Enum = actions_e
    actions : Dict[Any, Callable] = field(init=False, default_factory=lambda: {
        actions_e.STRIPQUOTE : lambda x: x.strip("\"'"),
        actions_e.KEYWORD    : lambda x: pp.Keyword(x),
        actions_e.LITERAL    : lambda x: pp.Literal(x),
        actions_e.LIST       : lambda x: x.split("\n"),
        actions_e.UNESCAPE   : lambda x: x.encode().decode("unicode_escape"),
        actions_e.SPLIT      : lambda x: x.split(" ")
    })
    instance : ClassVar['AcabConfig']                   = field(init=False, default=None)

    @staticmethod
    def Get(*paths: List[str]):
        if paths is None:
            paths = []
        if AcabConfig.instance is None:
            AcabConfig(paths)
        else:
            AcabConfig.instance.read(paths)

        return AcabConfig.instance

    def __post_init__(self, paths: List[str]):
        if AcabConfig.instance is not None:
            raise AcabConfigException("AcabConfig Already Exists")

        AcabConfig.instance = self

        self._config = ConfigParser(interpolation=ExtendedInterpolation(), allow_no_value=True)

        self.read(paths)

        # TODO: use __mro__?
        super().__post_init__()


    def read(self, paths: List[str]):
        full_paths = []
        for path in paths:
            expanded = abspath(expanduser(path))
            if isfile(expanded):
                full_paths.append(expanded)
                continue

            assert(isdir(expanded)), breakpoint()
            found = [join(expanded, x) for x in listdir(expanded)]
            full_paths += [x for x in found if splitext(x)[1] == ".config"]

        self._config.read(full_paths)
        self._files.update(full_paths)
        return self

    def __call__(self, lookup):
        return self.value(*lookup)

    def value(self, section, key=None, actions=None, as_list=None, as_dict=None):
        """
        Get use a lookup tuple at run time
        """
        if as_list and key is None:
            return list(self._config[section].keys())
        elif as_dict and key is None:
            return dict(self._config[section].items())

        # try:
        value = None
        in_file = section in self._config
        in_section = in_file and (key is None or key in self._config[section])

        if in_section:
            value = self._config[section][key]
        else:
            raise Exception("missing util value: {} {}".format(section, key))

        if value is None:
            value = key

        if actions is None:
            actions = []

        for action in actions:
            value = self.actions[action](value)

        return value



    def prepare(self, section, key=None, actions=None, as_list=False, as_dict=False):
        """
        Returns an accessor tuple for later,
        while guaranteeing the section:key:value does exist

        This lets AcabPrintSemantics be a proxy with AcabPrintSemantics.use
        """
        in_file = section in self._config
        in_section = in_file and (key is None or key in self._config[section])

        if in_section:
            return (section, key, actions, as_list, as_dict)

        raise Exception("missing util value: {} {}".format(section, key))

    @property
    def loaded(self):
        return bool(self._files)
