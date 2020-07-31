from .acab_base_exception import AcabBaseException


class AcabImportException(AcabBaseException):
    """  """

    def __init__(self, module_name):
        self._name = module_name

    def __str__(self):
        return "Import Failed: {}".format(self._name)


