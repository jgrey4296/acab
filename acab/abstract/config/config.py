""" Cross-module utilities for the rule engines """
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir

from configparser import ConfigParser, ExtendedInterpolation
from enum import Enum
import pyparsing as pp

from acab.error.acab_config_exception import AcabConfigException

actions_e = Enum("Config Actions", "STRIPQUOTE KEYWORD LITERAL DICT LIST UNESCAPE SPLIT")

def GET(*args):
    config = AcabConfig.Get(*args)
    return config

class AcabConfig:
    """ A Singleton class for the active configuration
    Uses ${SectionName:Key} interpolation in values,
    Turns multi-line values into lists
    """
    actions_e = actions_e
    actions = {
        actions_e.STRIPQUOTE : lambda x: x.strip("\"'"),
        actions_e.KEYWORD    : lambda x: pp.Keyword(x),
        actions_e.LITERAL    : lambda x: pp.Literal(x),
        actions_e.LIST       : lambda x: x.split("\n"),
        actions_e.UNESCAPE   : lambda x: x.encode().decode("unicode_escape"),
        actions_e.SPLIT      : lambda x: x.split(" ")
        }
    instance = None

    @staticmethod
    def Get(*paths):
        if AcabConfig.instance is None:
            AcabConfig()
        AcabConfig.instance.read(*paths)

        return AcabConfig.instance

    def __init__(self, *paths):
        if AcabConfig.instance is not None:
            raise AcabConfigException("AcabConfig Already Exists")

        AcabConfig.instance = self

        self._config = ConfigParser(interpolation=ExtendedInterpolation(), allow_no_value=True)
        self._files = set()

        self.read(*paths)

    def read(self, *paths):
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
            value = AcabConfig.actions[action](value)

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
