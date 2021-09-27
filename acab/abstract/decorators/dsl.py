from acab.error.acab_parse_exception import AcabParseException

def EnsureDSLInitialised(method):
    """ Utility Decorator for DSL Builder's, raising error if not initialised """
    def fn(self, *args, **kwargs):
        if not self._parsers_initialised:
            raise AcabParseException("DSL Not Initialised")

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn
