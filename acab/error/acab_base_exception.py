class AcabBaseException(Exception):
    """ The base exception class for the Acab package """

    def __init__(self, init_str=None):
        self._str = init_str

    def __str__(self):
        return "Non-specific Error Raised: {}".format(self._str)

