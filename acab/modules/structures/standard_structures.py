from acab.abstract.interfaces.dsl import DSL_Fragment_i

from . import agenda as AP
from . import layer as LP
from . import pipeline as PP

# TODO is this needed if the modules are imported above?
class StandardStructures(DSL_Fragment_i):
    """ The Module Spec for base structures
    ie: Agenda/Layer/Pipeline
    """

    def __init__(self):
        super().__init__()
        self._modules = [AP.MODULE(),
                      LP.MODULE(),
                      PP.MODULE()]

    def assert_parsers(self, pt):
        dummy = [x.assert_parsers(pt) for x in self._modules]

    def query_parsers(self, pt):
        dummy = [x.query_parsers(pt) for x in self._modules]
