import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.parsing.trie_bootstrapper import TrieBootstrapper
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionComponent, ProductionContainer, ProductionStructure
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
            return 1

    def tearDown(self):
            return 1

    #----------
    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(RP)

    def test_empty_rule_body(self):
        result = RP.rule_body.parseString("")[0]
        self.assertIsInstance(result, ProductionStructure)

    @unittest.skip
    def test_name_empty_rule_parse(self):
        result = RP.rule.parseString("a.rule.x: end")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertEqual(result[0][-1].name, "x")

    @unittest.skip
    def test_multi_empty_rules(self):
        result = RP.parseString("a.rule.x:\n\nend\n\na.second.rule:\n\nend")
        self.assertEqual(len(result),2)
        self.assertTrue(all([isinstance(x[-1], ProductionStructure) for x in result]))

    def test_rule_with_query(self):
        result = RP.parseString("a.rule.x:\na.b.c?\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)

    def test_rule_with_multi_clause_query(self):
        result = RP.parseString("a.rule.x:\na.b.c?\na.b.d?\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)
        self.assertEqual(len(result[0][-1][QUERY_V]), 2)

    def test_rule_with_multi_clauses_in_one_line(self):
        result = RP.parseString("a.rule.x:\na.b.c?, a.b.d?\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)
        self.assertEqual(len(result[0][-1][QUERY_V]), 2)

    def test_rule_with_binding_query(self):
        result = RP.parseString("a.rule.x:\na.b.$x?\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)
        self.assertEqual(len(result[0][-1][QUERY_V]), 1)

    def test_rule_with_actions(self):
        result = RP.parseString("a.rule.x:\nλoperator.action.add a.b.c\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 1)

    def test_multi_action_rule(self):
        result = RP.parseString("a.rule.x:\nλoperator.action.add a.b.c\nλoperator.action.add ~a.b.d\nend")
        self.assertEqual(len(result[0]), 3)
        self.assertIsInstance(result[0], Sentence)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 2)

    def test_multi_action_single_line_rule(self):
        result = RP.parseString("a.rule.x:\n\nλoperator.action.add a.b.c, λoperator.action.add ~a.b.d\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 2)
