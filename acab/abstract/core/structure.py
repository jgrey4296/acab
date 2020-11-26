from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from re import Pattern
from uuid import uuid1, UUID
from weakref import ref
import logging as root_logger


@dataclass
class DataStructure:
    """
    The Abstract DataStructure Class
    Anything that wants to use StructureSemantics has to fulfill this
    Like AcabNode, is not a value, thus not directly able to be talked about
    """

    semantics : 'AcabStructureSemantics' = field(default=None)
    uuid : UUID                          = field(default_factory=uuid1)
    root : 'AcabNode'                    = field(init=False)

    def __post_init__(self):
        assert(self.semantics is not None)
        self.root = self.semantics.make_root()

    def __bool__(self):
        return bool(self.root)
