from py_rule.abstract.module_interface import ModuleInterface

from . import agendas as AA
from . import layer as LA
from . import pipeline as PA
from . import query as QO
from . import action as A
from . import transform as T
from . import pattern_match as PMM


class StandardOperators(ModuleInterface):
    """ The Module Spec for base operators """

    def __init__(self):
        super().__init__()
        self._modules = [QO.MODULE(),
                         A.MODULE(),
                         AA.MODULE(),
                         LA.MODULE(),
                         PA.MODULE(),
                         T.MODULE(),
                         PMM.MODULE()]

    def assert_parsers(self, pt):
        dummy = [x.assert_parsers(pt) for x in self._modules]

    def query_parsers(self, pt):
        dummy = [x.query_parsers(pt) for x in self._modules]

    def init_strings(self):
        return []
