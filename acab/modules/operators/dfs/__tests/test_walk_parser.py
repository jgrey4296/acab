#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as logmod
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = logmod.getLogger(__name__)


import acab

config = acab.setup()

from acab.core.data.instruction import ProductionComponent, ProductionContainer
from acab.core.data.value import AcabValue
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence

from acab.modules.engines.configured import exlo
from acab.modules.operators.dfs import parser as DOP
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.statements import QueryPlusAbstraction
from acab.modules.semantics.values import ExclusionNodeSemantics

BIND          = config.prepare("Value.Structure", "BIND")()
QUERY         = config.prepare("Value.Structure", "QUERY")()
SEM_HINT      = config.prepare("Value.Structure", "SEMANTIC_HINT")()
TYPE_INSTANCE = config.prepare("Value.Structure", "TYPE_INSTANCE")()
AT_BIND       = config.prepare("Value.Structure", "AT_BIND")()
default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

class TestWalkParser(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        logmod.getLogger('').setLevel(logmod.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = logmod.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(logmod.DEBUG)

        console = logmod.StreamHandler()
        console.setLevel(logmod.WARNING)

        logging = logmod.getLogger(__name__)
        logging.setLevel(logmod.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)

    def test_parse_walk_query_instruction(self):
        result = DOP.dfs_query.parse_string("ᛦ $x(::blah)?")[0]

        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], '_:WALK')
        self.assertTrue(result[-1].data['QUERY'])

    def test_parse_walk_action_instruction(self):
        result = DOP.dfs_action.parse_string("ᛦ λa.test.op")[0]

        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], '_:WALK')
        self.assertNotIn('QUERY', result[-1].data)

        self.assertEqual(result[0], "_:a.test.op")

    def test_parse_walk_action_with_var(self):
        result = DOP.dfs_action.parse_string("ᛦ λ$x")[0]
        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], '_:WALK')
        self.assertNotIn('QUERY', result[-1].data)
        self.assertTrue(result[0][0].is_var)


