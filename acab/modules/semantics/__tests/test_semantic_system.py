#!/opt/anaconda3/envs/acab/bin/python
import sys
import unittest
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))

import acab

config = acab.setup()

from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.abstract.core.values import Sentence
from acab.abstract.interfaces.handler_system import Handler
from acab.abstract.interfaces.semantic import (AbstractionSemantics_i,
                                               SemanticSystem_i)
from acab.error.acab_base_exception import AcabBaseException
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.context.context_set import ContextInstance, ContextSet
from acab.modules.semantics.independent import ExclusionNodeSemantics

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V   = config.prepare("Value.Structure", "NEGATION")()
BIND_V       = config.prepare("Value.Structure", "BIND")()
CONSTRAINT_V = config.prepare("Value.Structure", "CONSTRAINT")()
QUERY_V      = config.prepare("Parse.Structure", "QUERY")()
TRANSFORM_V  = config.prepare("Parse.Structure", "TRANSFORM")()
ACTION_V     = config.prepare("Parse.Structure", "ACTION")()

SEMANTIC_HINT_V = config.prepare("Value.Structure", "SEMANTIC_HINT")()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])


class SemanticSystemTests(unittest.TestCase):

    class StubAbsSemantic(AbstractionSemantics_i):
        def __call__(self, ins, semSys, ctxs=None, data=None):
            raise AcabBaseException("TestAbsSem called")

    def test_construction(self):
        semsys = BasicSemanticSystem(default=SemanticSystemTests.StubAbsSemantic().as_handler("_:stub"))
        self.assertIsInstance(semsys, SemanticSystem_i)
        self.assertIsInstance(semsys.default.func, AbstractionSemantics_i)
        self.assertFalse(semsys.handlers)

    def test_default_call(self):
        semsys = BasicSemanticSystem(default=SemanticSystemTests.StubAbsSemantic().as_handler("_:stub"))
        test_sen = Sentence.build(["test"])
        with self.assertRaises(AcabBaseException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception.detail, "TestAbsSem called")

    def test_retrieval(self):
        # put some semantics in semsys.mapping
        semsys = BasicSemanticSystem(default=SemanticSystemTests.StubAbsSemantic().as_handler("_:stub"))


    def test_failure(self):
        # put a callable in failure
        pass

    def test_hooks(self):
        pass

if __name__ == '__main__':
    unittest.main()
