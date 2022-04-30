#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as logmod
import unittest
import unittest.mock as mock
from os.path import split, splitext
import pyparsing as pp

logging = logmod.getLogger(__name__)

if '@pytest_ar' in globals():
    from acab.core.parsing import debug_funcs as DBF
    DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

import acab

config = acab.setup()

from acab.core.value.instruction import ProductionComponent, ProductionContainer
from acab.core.value.value import AcabValue
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence

from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.core.parsing.pyparse_dsl import PyParseDSL
from acab.modules.engines.configured import exlo
from acab.modules.operators.dfs import parser as DOP
from acab.modules.operators.dfs.semantics import DFSSemantics
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.statements import QueryPlusAbstraction
from acab.modules.semantics.values import ExclusionNodeSemantics
from acab.modules.operators.dfs.module import DFS_DSL

BIND          = config.prepare("Value.Structure", "BIND")()
QUERY         = config.prepare("Value.Structure", "QUERY")()
SEM_HINT      = config.prepare("Value.Structure", "SEMANTIC_HINT")()
TYPE_INSTANCE = config.prepare("Value.Structure", "TYPE_INSTANCE")()
AT_BIND       = config.prepare("Value.Structure", "AT_BIND")()
default_modules = config.prepare("Module.REPL", "MODULES")().split("\n")

dsl = None

class TestWalkParser(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        global dsl
        dsl = PyParseDSL([],[],[])
        dsl.register(EXLO_Parser)
        dsl.register(DFS_DSL)
        dsl.build()

    def test_parse_walk_query_instruction(self):
        result = DOP.dfs_query.parse_string("ᛦ $x(λblah)?")[0]

        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], '_:WALK')
        self.assertTrue(result.data['QUERY'])

    def test_parse_walk_action_instruction(self):
        result = DOP.dfs_action.parse_string("ᛦ λa.test.op")[0]

        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], '_:WALK')
        self.assertNotIn('QUERY', result.data)

        self.assertEqual(result[0], "_:a.test.op")

    def test_parse_walk_action_with_var(self):
        result = DOP.dfs_action.parse_string("ᛦ λ$x")[0]
        self.assertTrue(result)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result.data['SEMANTIC_HINT'], '_:WALK')
        self.assertNotIn('QUERY', result.data)
        self.assertTrue(result[0][0].is_var)
