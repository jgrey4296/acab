from dataclasses import dataclass, field
from weakref import WeakValueDictionary
import logging as root_logger

from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces.data_interfaces import StructureInterface
from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import AcabValue

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

class AcabStruct(StructureInterface):
    """ A collection of nodes """
    pass

class BasicNodeStruct(AcabStruct):

    def build_default():
        struct = BasicNodeStruct(AcabNode.Root())
        struct.components['all_nodes'] = WeakValueDictionary()
        return struct

    def __bool__(self):
        return bool(self.root)

    def __len__(self):
        return len(self.components['all_nodes'])

class NPArrayStruct(AcabStruct):
    """ A numpy based data structure """
    pass

class NXGraphStruct(AcabStruct):
    """ A networkX graph based structure """
    pass
