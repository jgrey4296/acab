from .acab_base_exception import AcabBaseException

class AcabParseException(AcabBaseException):
    """ The base exception for parsing errors """

    def __init__(self, s=None):
        self.s=s


