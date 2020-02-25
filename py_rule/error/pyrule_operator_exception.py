from .pyrule_base_exception import PyRuleBaseException


class PyRuleOperatorException(PyRuleBaseException):

    def __init__(self, s):
        self._str = s
