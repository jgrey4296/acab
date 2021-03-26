from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces.data_interfaces import StructureInterface
from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import AcabValue

config = AcabConfig.Get()
ROOT = AcabValue(name=config.value("Data", "ROOT"))

class AcabStruct(StructureInterface):
    """ A collection of nodes """

    def __init__(self):
        pass

class BasicNodeStruct(AcabStruct):

    @staticmethod
    def build_default():
        return BasicNodeStruct(AcabNode.Root())

    def __init__(self, root):
        self.root = root

class NPArrayStruct(AcabStruct):
    pass

class NXGraphStruct(AcabStruct):
    pass
