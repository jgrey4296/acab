#!/opt/anaconda3/envs/acab/bin/python
import sys
import unittest
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))

import acab

config = acab.setup()

from acab.core.data.production_abstractions import (ProductionComponent,
                                                    ProductionContainer,
                                                    ProductionOperator,
                                                    ProductionStructure)
from acab.core.data.values import Sentence
from acab.interfaces.handler_system import Handler
from acab.interfaces.semantic import (StatementSemantics_i,
                                      SemanticSystem_i)
import acab.error.acab_exception as AE
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.context.context_set import ContextInstance, ContextSet
from acab.modules.semantics.values import ExclusionNodeSemantics

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

    class StubAbsSemantic(StatementSemantics_i):
        def __call__(self, ins, semSys, ctxs=None, data=None):
            raise AE.AcabBasicException("TestAbsSem called")

    def test_construction(self):
        """ Check context systems can be created """
        semsys = BasicSemanticSystem(default=SemanticSystemTests.StubAbsSemantic().as_handler("_:stub"))
        self.assertIsInstance(semsys, SemanticSystem_i)
        self.assertIsInstance(semsys.default.func, StatementSemantics_i)
        self.assertFalse(semsys.handlers)

    def test_default_call(self):
        """ Check context systems can be called """
        semsys = BasicSemanticSystem(default=SemanticSystemTests.StubAbsSemantic().as_handler("_:stub"))
        test_sen = Sentence.build(["test"])
        with self.assertRaises(AE.AcabException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception.detail, "TestAbsSem called")

    def test_retrieval(self):
        """ Check context systems can lookup the correct semantics for an input """
        # put some semantics in semsys.mapping
        semsys = BasicSemanticSystem(default=SemanticSystemTests.StubAbsSemantic().as_handler("_:stub"))


    def test_failure(self):
        # put a callable in failure
        pass

    def test_hooks(self):
        pass

if __name__ == '__main__':
    unittest.main()
