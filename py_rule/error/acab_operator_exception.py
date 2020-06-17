from .acab_base_exception import AcabBaseException


class AcabOperatorException(AcabBaseException):

    def __init__(self, s):
        self._str = s
