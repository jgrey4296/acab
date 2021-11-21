import unittest
from os.path import splitext, split
import logging as root_logger
import pyparsing as pp
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.core.data.values import AcabValue
from acab.core.data.values import Sentence
from acab.core.parsing.trie_bootstrapper import TrieBootstrapper
from acab.core.data.production_abstractions import ProductionOperator, ProductionComponent, ProductionContainer, ProductionStructure
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP
from acab.modules.parsing.exlo.parsers import RuleParser as RP
from acab.modules.parsing.exlo.parsers import QueryParser as QP

QUERY_V     = config.prepare("Structure.Components", "QUERY")()
TRANSFORM_V = config.prepare("Structure.Components", "TRANSFORM")()
ACTION_V    = config.prepare("Structure.Components", "ACTION")()

# TODO rule:

class Trie_Rule_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)


    def setUp(self):
        FP.HOTLOAD_SEN_ENDS <<= QP.query_sen_end | pp.NoMatch()

    def tearDown(self):
        FP.HOTLOAD_SEN_ENDS <<= pp.NoMatch()

    #----------
    #use testcase snippets
    def test_init(self):
        """ Check the basic rule parser exists """
        self.assertIsNotNone(RP)

    def test_empty_rule_body(self):
        """ Check a rule can be empty """
        result = RP.rule_body.parseString("")[0]
        self.assertIsInstance(result, ProductionStructure)

    def test_name_empty_rule_parse(self):
        result = RP.rule.parseString("x(::ρ):\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertEqual(result.name, "x")


    @unittest.skip
    def test_multi_empty_rules(self):
        result = RP.parseString("x(::ρ):\n\nend\n\na.second.rule:\n\nend")
        self.assertEqual(len(result),2)
        self.assertTrue(all([isinstance(x[-1], ProductionStructure) for x in result]))

    def test_rule_with_query(self):
        result = RP.rule.parseString("x(::ρ):\n a.b.c?\nend")[0]
        self.assertEqual(result.name, "x")
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)


    def test_rule_with_multi_clause_query(self):
        result = RP.parseString("x(::ρ):\n  a.b.c?\n  a.b.d?\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)
        self.assertEqual(len(result[QUERY_V]), 2)


    @unittest.skip("one liners need work")
    def test_rule_with_multi_clauses_in_one_line(self):
        result = RP.parseString("x(::ρ):\n a.b.c?, a.b.d?\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)
        self.assertEqual(len(result[QUERY_V]), 2)


    def test_rule_with_binding_query(self):
        result = RP.parseString("x(::ρ):\na.b.$x?\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)
        self.assertEqual(len(result[QUERY_V]), 1)


    def test_rule_with_actions(self):
        result = RP.parseString("x(::ρ):\nλoperator.action.add a.b.c\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNone(result[QUERY_V])
        self.assertIsNone(result[TRANSFORM_V])
        self.assertEqual(len(result[ACTION_V]), 1)


    def test_multi_action_rule(self):
        result = RP.parseString("x(::ρ):\n  λoperator.action.add a.b.c\n  λoperator.action.add ~a.b.d\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNone(result.structure[QUERY_V])
        self.assertIsNone(result.structure[TRANSFORM_V])
        self.assertEqual(len(result.structure[ACTION_V]), 2)

    @unittest.skip("one liners need work")
    def test_multi_action_single_line_rule(self):
        result = RP.parseString("x(::ρ):\n λoperator.action.add a.b.c, λoperator.action.add ~a.b.d\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 2)

    def test_query_and_transform_rule(self):
        result = RP.rule.parseString("x(::ρ):\n  a.b.c?\n  d.e.f?\n\n  λa.b.c $x -> $y\nend")[0]
        self.assertIsInstance(result, ProductionStructure)

    def test_query_and_transform_and_action_rule(self):
        result = RP.rule.parseString("rule(::ρ):\n  a.b.c?\n  d.e.f?\n\n  λa.b.c $x -> $y\n\n a.b.c\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
