from functools import wraps
from acab.error.parse_exception import AcabParseException

def EnsureDSLInitialised(method):
    """ Utility Decorator for DSL Builder's, raising error if not initialised """
    @wraps(method)
    def fn(self, *args, **kwargs):
        if not self._parsers_initialised:
            raise AcabParseException("DSL Not Initialised")

        return method(self, *args, **kwargs)

    return fn
