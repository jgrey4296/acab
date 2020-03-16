from .pyrule_base_exception import PyRuleBaseException

class PyRuleParseException(PyRuleBaseException):
    """ The base exception for parsing errors """

    def __init__(self, s=None):
        self.s=s


