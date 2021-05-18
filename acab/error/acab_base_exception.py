class AcabBaseException(Exception):
    """ The base exception class for the Acab package """

    def __init__(self, init_str=None, *args):
        self._str = init_str
        self._args = args

    def __str__(self):
        return "Non-specific Error Raised: {} : {}".format(self._str,
                                                           self._args)
