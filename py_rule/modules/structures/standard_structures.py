from py_rule.abstract.module_interface import ModuleInterface

from . import agenda as AP
from . import layer as LP
from . import pipeline as PP


class StandardStructures(ModuleInterface):
    """ The Module Spec for base structures
    ie: Agenda/Layer/Pipeline
    """

    def __init__(self):
        super().__init__()
        self._modules = [AP.MODULE(),
                         LP.MODULE(),
                         PP.MODULE()]

    def init_strings(self):
        return []

    def assert_parsers(self, pt):
        dummy = [x.assert_parsers(pt) for x in self._modules]

    def query_parsers(self, pt):
        dummy = [x.query_parsers(pt) for x in self._modules]
