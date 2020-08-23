""" Cross-module utilities for the rule engines """
from configparser import ConfigParser, ExtendedInterpolation
from acab.error.acab_config_exception import AcabConfigException
from enum import Enum

# Provide accessor and setup from config file

class AcabConfig:
    """ A Singleton class for the active configuration
    Uses ${SectionName:Key} interpolation in values,
    Turns multi-line values into lists
    """

    actions_e = Enum("Config Actions", "STRIPQUOTE")
    actions = {
        AcabConfig.actions_e.STRIP : lambda x: x.strip("'")
        }
    instance = None

    @staticmethod
    def Get():
        if AcabConfig.instance is None:
            AcabConfig()
        return AcabConfig.instance

    def __init__(self, *paths):
        if AcabConfig.instance is not None:
            raise AcabConfigException("AcabConfig Already Exists")

        AcabConfig.instance = self

        self._config = ConfigParser(interpolation=ExtendedInterpolation())
        self._files = set()

        for path in paths:
            self.read(path)

    def read(self, path, force=False):
        if force or path not in self._files:
            self._config.read(path)
            self._files.add(path)

    def __call__(self, section, key, action=None):
        """
        Get a value from the config
        """
        value = self.config[section][key]
        if action in AcabConfig.actions:
            value = AcabConfig.actions[action](value)

        return value
