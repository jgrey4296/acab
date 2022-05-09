import logging as logmod
from dataclasses import dataclass, field
from weakref import WeakValueDictionary

from acab.core.config.config import AcabConfig
from acab.core.data.node import AcabNode
from acab.core.util.decorators.util import cache
from acab.interfaces.data import Structure_i

logging = logmod.getLogger(__name__)
config  = AcabConfig()

class AcabStruct(Structure_i):
    """ A structure in ACAB,
    which is registered into the semantic system for use """
    pass

class BasicNodeStruct(AcabStruct):
    """ A Node based struct """

    @staticmethod
    def build_default():
        logging.info(f"Building Node Struct")
        struct = BasicNodeStruct(AcabNode.Root())
        # all_nodes : WeakDict[UUID, Node]
        struct.components['all_nodes'] = WeakValueDictionary()
        return struct

    def __bool__(self) -> bool:
        return bool(self.root)

    def __len__(self) -> int:
        return len(self.components['all_nodes'])

    def __repr__(self) -> str:
        val = ["BasicNodeStruct("]
        val.append(";".join([x.name for x in self.components['all_nodes'].values()][:5]))
        val.append("...)")
        return "".join(val)

    def __contains__(self, other) -> bool:
        if not isinstance(other, AcabNode):
            return False

        return other in self.componenents

    def __delitem__(self, key): pass
    def __getitem__(self, key): pass
    def __setitem__(self, key, value): pass
    def __iter__(self): pass
