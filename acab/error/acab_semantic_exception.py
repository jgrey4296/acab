from .acab_base_exception import AcabBaseException

class AcabSemanticException(AcabBaseException):
    """ The Core exception report of semantic operations  """

    def __init__(self, info, fail_clause):
        self._info = info
        self._data = fail_clause

    def __str__(self):
        return "Semantic Failure: {} at: {}".format(self._info, str(self._data))
