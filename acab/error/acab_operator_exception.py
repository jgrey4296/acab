from .acab_base_exception import AcabBaseException


class AcabOperatorException(AcabBaseException):

    def __init__(self, op):
        self._op = op

    def __str__(self):
        return "Invalid Operator Specified: {}".format(str(self._op))
