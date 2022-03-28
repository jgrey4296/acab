#!/usr/bin/env python3
import logging as logmod

import acab.interfaces.semantic as SI
import acab.error.semantic as ASErr
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence


logging = logmod.getLogger(__name__)
config = AcabConfig()

CONSTRAINT_S     = config.prepare("Value.Structure", "CONSTRAINT")()
NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()
QUERY_FALLBACK_S = config.prepare("Value.Structure", "QUERY_FALLBACK")()
DEFAULT_SETUP_S  = config.prepare("Data", "DEFAULT_SETUP_METHOD")()
DEFAULT_UPDATE_S = config.prepare("Data", "DEFAULT_UPDATE_METHOD")()

Node          = 'AcabNode'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'AcabStruct'
Engine        = 'Engine'
Contexts      = 'Contexts'


class ASPSemantics(SI.StructureSemantics_i):
    """
    Stub for passing assertions and queries into an ASP program
    """

    def insert(self, struct, sen, data=None, ctxs=None):
        """
        construct the ASP program
        """
        pass


    def query(self, struct, query, data=None, ctxs=None):
        """
        pass the cached asp program to a solver,
        retrieve results, extract what is needed,
        and return as sentences
        """
        pass

    def trigger(self, struct, sen, data=None, ctxs=None):
        pass
