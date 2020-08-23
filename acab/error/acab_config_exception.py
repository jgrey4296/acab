from .acab_base_exception import AcabBaseException

class AcabConfigException(AcabBaseException):
    """ Exceptions relating to configuration"""

    def __init__(self, config_info):
        self._info = config_info

    def __str__(self):
        return "Configuration Failure: {}".format(self._info)
