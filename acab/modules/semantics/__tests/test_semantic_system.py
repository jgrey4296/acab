#!/opt/anaconda3/envs/acab/bin/python
import logging as logmod
import sys
import unittest
from os.path import abspath, expanduser, split, splitext

logging = logmod.getLogger(__name__)

sys.path.append(abspath(expanduser("~/github/acab")))

import acab

config = acab.setup()

import acab.error.base as AE
from acab.core.semantics import basic
from acab.core.value.instruction import (ProductionComponent,
                                         ProductionContainer,
                                         ProductionOperator,
                                         ProductionStructure)
from acab.core.value.sentence import Sentence
from acab.interfaces.handler_system import Handler_i
from acab.interfaces.semantic import SemanticSystem_i, StatementSemantics_i
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.values import ExclusionNodeSemantics

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

EXOP         = config.attr.MODAL.exop
EXOP_enum    = config.prepare(EXOP, as_enum=True)()

NEGATION_V   = config.attr.Value.Structure.NEGATION
BIND_V       = config.attr.Value.Structure.BIND
CONSTRAINT_V = config.attr.Value.Structure.CONSTRAINT
QUERY_V      = config.attr.Parse.Structure.QUERY
TRANSFORM_V  = config.attr.Parse.Structure.TRANSFORM
ACTION_V     = config.attr.Parse.Structure.ACTION

SEMANTIC_HINT_V = config.attr.Value.Structure.SEMANTIC_HINT

QUERY_SIGNAL     = Sentence() << config.attr.Semantic.Signals.QUERY
ACTION_SIGNAL    = Sentence() << config.attr.Semantic.Signals.ACTION
TRANSFORM_SIGNAL = Sentence() << config.attr.Semantic.Signals.TRANSFORM
RULE_SIGNAL      = Sentence() << config.attr.Semantic.Signals.RULE
AGENDA_SIGNAL    = Sentence() << config.attr.Semantic.Signals.AGENDA
LAYER_SIGNAL     = Sentence() << config.attr.Semantic.Signals.LAYER
PIPELINE_SIGNAL  = Sentence() << config.attr.Semantic.Signals.PIPELINE

class StubAbsSemantic(basic.StatementSemantics, StatementSemantics_i):
    def __call__(self, ins, semSys, ctxs=None, data=None):
        raise AE.AcabBasicException("TestAbsSem called")

class SemanticSystemTests(unittest.TestCase):


    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

    def test_construction(self):
        """ Check context systems can be created """
        semsys = BasicSemanticSystem(init_handlers=[StubAbsSemantic().as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        self.assertIsInstance(semsys, SemanticSystem_i)
        self.assertIsInstance(semsys.lookup()[0], StatementSemantics_i)
        self.assertTrue(semsys.handler_specs)

    def test_default_call(self):
        """ Check context systems can be called """
        semsys = BasicSemanticSystem(init_handlers=[StubAbsSemantic().as_handler(signal=DEFAULT_HANDLER_SIGNAL)])
        test_sen = Sentence(["test"])
        with self.assertRaises(AE.AcabException) as cm:
            semsys(test_sen)

        self.assertEqual(cm.exception.detail, "TestAbsSem called")

    @unittest.skip("Not Implemented")
    def test_retrieval(self):
        """ Check context systems can lookup the correct semantics for an input """
        # put some semantics in semsys.mapping
        semsys = BasicSemanticSystem(init_handlers=[StubAbsSemantic().as_handler(signal=DEFAULT_HANDLER_SIGNAL)])


    @unittest.skip("not implemented")
    def test_failure(self):
        # put a callable in failure
        pass

    @unittest.skip("not implemented")
    def test_hooks(self):
        pass

if __name__ == '__main__':
    unittest.main()
