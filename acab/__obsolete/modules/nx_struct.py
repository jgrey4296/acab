from dataclasses import dataclass, field
from weakref import WeakValueDictionary
import logging as logmod

import networkx as nx
from acab.core.config.config import AcabConfig
from acab.interfaces.data import Structure_i
from acab.core.data.node import AcabNode
from acab.core.data.acab_struct import AcabStruct

logging = logmod.getLogger(__name__)
config  = AcabConfig()

class NXGraphStruct(AcabStruct):
    """ A Node based struct """

    @staticmethod
    def build_default():
        logging.info(f"Building Node Struct")
        struct = NXGraphStruct(AcabNode.Root())
        # all_nodes : WeakDict[UUID, Node]
        struct.components['all_nodes'] = WeakValueDictionary()
        return struct

    def __bool__(self):
        return bool(self.root)

    def __len__(self):
        return len(self.components['all_nodes'])

    def __repr__(self):
        val = f"NXGraphStruct("
        val += ";".join([x.name for x in self.components['all_nodes'].values()][:5])
        val += "..."
        val += ")"
        return val

    def __delitem__(self, key):
        pass

    def __getitem__(self, key):
        pass

    def __setitem__(self, key, value):
        pass

    def __iter__(self):
        pass


