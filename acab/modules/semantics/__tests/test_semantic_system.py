#!/opt/anaconda3/envs/acab/bin/python
import sys
from os.path import abspath, expanduser
import unittest

sys.path.append(abspath(expanduser("~/github/acab")))

import acab

config = acab.setup()

from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.abstract.core.values import Sentence
from acab.abstract.interfaces.semantic_interfaces import (AbstractionSemantics,
                                                          SemanticSystem)
from acab.error.acab_base_exception import AcabBaseException
from acab.modules.semantics.context_container import (ContextContainer,
                                                      ContextInstance)
from acab.modules.semantics.independent import ExclusionNodeSemantics
from acab.modules.semantics.system import BasicSemanticSystem

EXOP         = config.value("MODAL", "exop")
EXOP_enum    = config.enums[EXOP]

NEGATION_V   = config.value("Value.Structure", "NEGATION")
BIND_V       = config.value("Value.Structure", "BIND")
CONSTRAINT_V = config.value("Value.Structure", "CONSTRAINT")
QUERY_V      = config.value("Parse.Structure", "QUERY")
TRANSFORM_V  = config.value("Parse.Structure", "TRANSFORM")
ACTION_V     = config.value("Parse.Structure", "ACTION")

SEMANTIC_HINT_V = config.value("Value.Structure", "SEMANTIC_HINT")

QUERY_SEM_HINT     = Sentence.build([config.value("SEMANTICS", "QUERY")])
ACTION_SEM_HINT    = Sentence.build([config.value("SEMANTICS", "ACTION")])
TRANSFORM_SEM_HINT = Sentence.build([config.value("SEMANTICS", "TRANSFORM")])
RULE_SEM_HINT      = Sentence.build([config.value("SEMANTICS", "RULE")])
AGENDA_SEM_HINT    = Sentence.build([config.value("SEMANTICS", "AGENDA")])
LAYER_SEM_HINT     = Sentence.build([config.value("SEMANTICS", "LAYER")])
PIPELINE_SEM_HINT  = Sentence.build([config.value("SEMANTICS", "PIPELINE")])

# TODO test verify

class SemanticSystemTests(unittest.TestCase):

    class StubAbsSemantic(AbstractionSemantics):
        def __call__(self, ins, ctxCon, semSys, data=None):
            raise AcabBaseException("TestAbsSem called")

    def test_construction(self):
        semsys = BasicSemanticSystem(SemanticSystemTests.StubAbsSemantic(), None)
        self.assertIsInstance(semsys, SemanticSystem)
        self.assertIsInstance(semsys.base, AbstractionSemantics)
        self.assertFalse(semsys.mapping)
        self.assertFalse(semsys.structs)

    def test_default_call(self):
        semsys = BasicSemanticSystem(SemanticSystemTests.StubAbsSemantic(), None)
        test_sen = Sentence.build(["test"])
        with self.assertRaises(AcabBaseException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception._str, "TestAbsSem called")

    def test_retrieval(self):
        # put some semantics in semsys.mapping
        semsys = BasicSemanticSystem(SemanticSystemTests.StubAbsSemantic(), None)



    def test_failure(self):
        # put a callable in failure
        pass

    def test_hooks(self):
        pass

if __name__ == '__main__':
    unittest.main()
