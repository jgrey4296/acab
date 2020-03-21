import unittest
import logging
from py_rule.working_memory.trie_wm import util as KBU
from py_rule.working_memory.trie_wm.parsing import RuleParser as RP
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.modules.operators.operator_module import OperatorSpec
from py_rule.abstract.rule import Rule
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.query import Query


class Trie_Rule_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        os = OperatorSpec()
        os._construct_comp_ops()
        os._construct_action_ops()
        os._construct_transform_ops()
        RP.build_operators()

    def setUp(self):
            return 1

    def tearDown(self):
            return 1

    #----------
    #use testcase snippets
    def test_init(self):
            self.assertIsNotNone(RP)

    def test_name_empty_rule_parse(self):
            result = RP.parseString("a.rule:\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertEqual(str(result[0][1]._name), "a.rule")

    def test_multi_empty_rules(self):
            result = RP.parseString("a.rule:\nend\n\na.second.rule:\nend")
            self.assertEqual(len(result),2)
            self.assertTrue(all([isinstance(x[1],Rule) for x in result]))

    def test_rule_with_query(self):
            result = RP.parseString("a.rule:\na.b.c?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNotNone(result[0][1]._query)
            self.assertIsInstance(result[0][1]._query, Query)

    def test_rule_with_multi_clause_query(self):
            result = RP.parseString("a.rule:\na.b.c?,\na.b.d?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNotNone(result[0][1]._query)
            self.assertIsInstance(result[0][1]._query, Query)
            self.assertEqual(len(result[0][1]._query), 2)

    def test_rule_with_multi_clauses_in_one_line(self):
            result = RP.parseString("a.rule:\na.b.c?, a.b.d?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNotNone(result[0][1]._query)
            self.assertIsInstance(result[0][1]._query, Query)
            self.assertEqual(len(result[0][1]._query), 2)

    def test_rule_with_binding_query(self):
            result = RP.parseString("a.rule:\na.b.$x?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNotNone(result[0][1]._query)
            self.assertIsInstance(result[0][1]._query, Query)
            self.assertEqual(len(result[0][1]._query), 1)

    def test_rule_with_actions(self):
            result = RP.parseString("a.rule:\nActionAdd(a.b.c)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNone(result[0][1]._query)
            self.assertIsNone(result[0][1]._transform)
            self.assertEqual(len(result[0][1]._action), 1)

    def test_multi_action_rule(self):
            result = RP.parseString("a.rule:\nActionAdd(a.b.c),\nActionRetract(a.b.d)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNone(result[0][1]._query)
            self.assertIsNone(result[0][1]._transform)
            self.assertEqual(len(result[0][1]._action), 2)

    def test_multi_action_single_line_rule(self):
            result = RP.parseString("a.rule:\nActionAdd(a.b.c), ActionRetract(a.b.d)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNone(result[0][1]._query)
            self.assertIsNone(result[0][1]._transform)
            self.assertEqual(len(result[0][1]._action), 2)

    def test_rule_simple_binding_expansion(self):
        bindings = { "x" : FP.parseString('a.b.c')[0] }
        result = RP.parseString("a.rule:\n$x?\n\nend")[0][1]
        expanded = result.expand_bindings(bindings)
        self.assertEqual(str(expanded),
                         "a.rule:\n\ta.b.c?\n\nend")

    def test_rule_tags(self):
            result = RP.parseString('a.test.rule:\n#blah, #bloo, #blee\n\na.b.c?\n\nActionAdd(a.b.c)\nend')[0][1]
            self.assertIsInstance(result, Rule)
            self.assertEqual(str(result._name), "a.test.rule")
            self.assertTrue(all(x in result._tags for x in ["blah","bloo","blee"]))


if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Rule_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
