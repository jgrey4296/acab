#!/opt/anaconda3/envs/acab/bin/python
import logging as logmod
import sys
import unittest
from enum import Enum
from os.path import abspath, expanduser, split, splitext

logging = logmod.getLogger(__name__)

sys.path.append(abspath(expanduser("~/github/acab")))

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    import acab.core.defaults.value_keys as DS
    import acab.error.base as AE
    from acab.core.semantics import basic
    from acab.core.util.sentences import ProductionComponent
    from acab.core.value.instruction import (ProductionContainer,
                                            ProductionOperator,
                                            ProductionStructure)
    from acab.core.value.sentence import Sentence
    from acab.interfaces.handler_system import Handler_i
    from acab.interfaces.semantic import SemanticSystem_i, StatementSemantics_i
    from acab.modules.semantics.basic_system import BasicSemanticSystem
    from acab.modules.semantics.values import ExclusionNodeSemantics

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

EXOP            = config.attr.MODAL.exop
EXOP_enum       = config.prepare(EXOP, _type=Enum)()

NEGATION_V      = DS.NEGATION
BIND_V          = DS.BIND
CONSTRAINT_V    = DS.CONSTRAINT
SEMANTIC_HINT_V = DS.SEMANTIC_HINT

CONTAINER_SEN          = Sentence() << config.attr.Type.Primitive.CONTAINER
STRUCT_SEN             = Sentence() << config.attr.Type.Primitive.STRUCTURE

QUERY_SIGNAL           = CONTAINER_SEN << config.attr.Type.Primitive.QUERY
ACTION_SIGNAL          = CONTAINER_SEN << config.attr.Type.Primitive.ACTION
TRANSFORM_SIGNAL       = CONTAINER_SEN << config.attr.Type.Primitive.TRANSFORM

RULE_SIGNAL            = STRUCT_SEN << config.attr.Type.Primitive.RULE

class StubAbsSemantic(basic.StatementSemantics, StatementSemantics_i):
    def __call__(self, ins, semSys, ctxs=None, data=None):
        raise AE.AcabBasicException("TestAbsSem called")

class SemanticSystemTests(unittest.TestCase):


    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

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

##-- ifmain
if __name__ == '__main__':
    unittest.main()
##-- end ifmain
