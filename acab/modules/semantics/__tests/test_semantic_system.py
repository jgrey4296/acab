#!/opt/anaconda3/envs/acab/bin/python
import sys
import unittest
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))

import acab

config = acab.setup()

from acab.core.value.instruction import (ProductionComponent,
                                                    ProductionContainer,
                                                    ProductionOperator,
                                                    ProductionStructure)
from acab.core.value.sentence import Sentence
from acab.interfaces.handler_system import Handler_i
from acab.interfaces.semantic import (StatementSemantics_i,
                                      SemanticSystem_i)
import acab.error.base as AE
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.core.semantics import basic
from acab.modules.context.context_set import ContextInstance, ContextSet
from acab.modules.semantics.values import ExclusionNodeSemantics

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V   = config.prepare("Value.Structure", "NEGATION")()
BIND_V       = config.prepare("Value.Structure", "BIND")()
CONSTRAINT_V = config.prepare("Value.Structure", "CONSTRAINT")()
QUERY_V      = config.prepare("Parse.Structure", "QUERY")()
TRANSFORM_V  = config.prepare("Parse.Structure", "TRANSFORM")()
ACTION_V     = config.prepare("Parse.Structure", "ACTION")()

SEMANTIC_HINT_V = config.prepare("Value.Structure", "SEMANTIC_HINT")()

QUERY_SEM_HINT     = Sentence([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence([config.prepare("SEMANTICS", "PIPELINE")()])

class StubAbsSemantic(basic.StatementSemantics, StatementSemantics_i):
    def __call__(self, ins, semSys, ctxs=None, data=None):
        raise AE.AcabBasicException("TestAbsSem called")

class SemanticSystemTests(unittest.TestCase):


    def test_construction(self):
        """ Check context systems can be created """
        semsys = BasicSemanticSystem(init_specs=[], sieve_fns=[], init_handlers=[StubAbsSemantic().as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        self.assertIsInstance(semsys, SemanticSystem_i)
        self.assertIsInstance(semsys.lookup()[0], StatementSemantics_i)
        self.assertTrue(semsys.handler_specs)

    def test_default_call(self):
        """ Check context systems can be called """
        semsys = BasicSemanticSystem(init_specs=[], sieve_fns=[], init_handlers=[StubAbsSemantic().as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        test_sen = Sentence(["test"])
        with self.assertRaises(AE.AcabException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception.detail, "TestAbsSem called")

    @unittest.skip("Not Implemented")
    def test_retrieval(self):
        """ Check context systems can lookup the correct semantics for an input """
        # put some semantics in semsys.mapping
        semsys = BasicSemanticSystem(init_specs=[], sieve_fns=[], init_handlers=[StubAbsSemantic().as_handler(signal=DEFAULT_HANDLER_SIGNAL)])


    @unittest.skip("not implemented")
    def test_failure(self):
        # put a callable in failure
        pass

    @unittest.skip("not implemented")
    def test_hooks(self):
        pass

if __name__ == '__main__':
    unittest.main()
