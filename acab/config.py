""" Cross-module utilities for the rule engines """
from configparser import ConfigParser, ExtendedInterpolation
from enum import Enum
import pyparsing as pp

from acab.error.acab_config_exception import AcabConfigException

actions_e = Enum("Config Actions", "STRIPQUOTE KEYWORD LITERAL DICT LIST UNESCAPE")

def GET(s=None):
    config = AcabConfig.Get(s)
    return config

class AcabConfig:
    """ A Singleton class for the active configuration
    Uses ${SectionName:Key} interpolation in values,
    Turns multi-line values into lists
    """
    actions_e = actions_e
    actions = {
        actions_e.STRIPQUOTE : lambda x: x.strip("\"'"),
        actions_e.KEYWORD : lambda x: pp.Keyword(x),
        actions_e.LITERAL : lambda x: pp.Literal(x),
        actions_e.LIST : lambda x: x.split("\n"),
        actions_e.UNESCAPE : lambda x: x.encode().decode("unicode_escape")
        }
    instance = None

    @staticmethod
    def Get(path=None):
        if AcabConfig.instance is None:
            AcabConfig()
        if path is not None:
            AcabConfig.instance.read(path)
        return AcabConfig.instance

    def __init__(self, *paths):
        if AcabConfig.instance is not None:
            raise AcabConfigException("AcabConfig Already Exists")

        AcabConfig.instance = self

        self._config = ConfigParser(interpolation=ExtendedInterpolation())
        self._files = set()

        self.read_list(paths)

    def read_list(self, lst):
        assert(isinstance(lst, (list, tuple)))
        for x in lst:
            self.read(x)

    def read(self, path, force=False):
        if force or path not in self._files:
            self._config.read(path)
            self._files.add(path)

    def __call__(self, section, key, *actions, action=None):
        """
        Get a value from the config
        """
        # try:
        value = self._config[section][key]
        # except KeyError as e:
            # breakpoint()

        if action in AcabConfig.actions:
            value = AcabConfig.actions[action](value)

        for action in actions:
            value = AcabConfig.actions[action](value)

        return value


    @property
    def loaded(self):
        return bool(self._files)
