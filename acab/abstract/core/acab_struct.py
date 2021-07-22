from dataclasses import dataclass, field
from weakref import WeakValueDictionary
import logging as root_logger

from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces.data_interfaces import Structure_i
from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import AcabValue

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

class AcabStruct(Structure_i):
    """ A structure in ACAB,
    which is registered into the semantic system for use """
    pass

class BasicNodeStruct(AcabStruct):
    """ A Node based struct """

    @staticmethod
    def build_default(name="_:Node"):
        struct = BasicNodeStruct(name, AcabNode.Root())
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
