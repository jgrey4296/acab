#!/opt/anaconda3/envs/acab/bin/python
import sys
import unittest
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))


import acab

config = acab.setup()

from acab.abstract.core.production_abstractions import ProductionContainer
from acab.modules.semantics.context_container import (ContextContainer,
                                                      ContextInstance)
from acab.modules.semantics.independent import ExclusionNodeSemantics

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V   = config.prepare("Value.Structure", "NEGATION")()
BIND         = config.prepare("Value.Structure", "BIND")()
CONSTRAINT_V = config.prepare("Value.Structure", "CONSTRAINT")()

class FSMSemanticTests(unittest.TestCase):
    def test_fsm_insert(self):
        pass

    def test_fsm_trigger(self):
        pass



class ASPSemanticTests(unittest.TestCase):
    pass

class SemanticHookTests(unittest.TestCase):
    pass

if __name__ == '__main__':
    unittest.main()
