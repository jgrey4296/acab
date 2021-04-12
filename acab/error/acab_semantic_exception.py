from .acab_base_exception import AcabBaseException

class AcabSemanticException(AcabBaseException):
    """ The Core exception report of semantic operations  """

    def __init__(self, info, fail_clause):
        self._info = info
        self._data = fail_clause

    def __str__(self):
        return "Semantic Failure: {} at: {}".format(self._info, str(self._data))

class AcabOperatorMissingException(AcabSemanticException):
    """ Raised when an operator can't be found when running semantics """
    def __init__(self, info, fail_clause):
        self._info = info
        self._data = fail_clause

    def __str__(self):
        return "Semantic Failure: {} at: {}".format(self._info, str(self._data))

class AcabSemanticTestFailure(AcabSemanticException):
    """ Raised by ConstraintCollection when a test fails,
    for ContextContainer to handle
    """

    def __init__(self, info, fail_clause):
        self._info = info
        self._data = fail_clause

    def __str__(self):
        return "Semantic Failure: {} at: {}".format(self._info, str(self._data))

class AcabSemanticQueryContextDepletionFailure(AcabSemanticException):
    """ Raised by Dependent Semantics to signal the current
    ContextInstance can't progress any further """

    def __init__(self, info, fail_clause):
        self._info = info
        self._data = fail_clause

    def __str__(self):
        return "Semantic Failure: {} at: {}".format(self._info, str(self._data))


class AcabSemanticIndependentFailure(AcabSemanticException):
    """ Signals failures in the node semantics """

    def __init__(self, info, fail_clause):
        self._info = info
        self._data = fail_clause

    def __str__(self):
        return "Semantic Failure: {} at: {}".format(self._info, str(self._data))
