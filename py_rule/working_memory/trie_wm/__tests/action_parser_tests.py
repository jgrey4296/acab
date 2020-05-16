import unittest
import logging

from py_rule.abstract.printing import util as PrU
from py_rule.abstract.bootstrap_parser import BootstrapParser
from py_rule.working_memory.trie_wm.parsing import ActionParser as AP
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.modules.operators.standard_operators import StandardOperators
from py_rule.abstract import action
from py_rule.abstract.production_operator import ProductionOperator


class Trie_Action_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        bp = BootstrapParser()
        os = StandardOperators()
        os.assert_parsers(bp)
        AP.HOTLOAD_OPERATORS << bp.query("operator.action.*")
        ProductionOperator.construct_subclass_tree()


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_string_value(self):
        result = AP.parseString('ActionAdd("blah bloo", "blee", "awef")')
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, action.Action)
        self.assertEqual(result.clauses[0].op, "ActionAdd")
        self.assertEqual([x[-1]._value for x in result.clauses[0]._params], ["blah bloo","blee","awef"])

    def test_actions_fact_str(self):
        result = AP.parseString('ActionAdd(a.b.c), ActionAdd(~a!b.d), ActionAdd($x), ActionAdd($x.a.b)')
        self.assertIsInstance(result, action.Action)
        self.assertEqual(len(result), 4)
        self.assertTrue(all([isinstance(x, action.ActionComponent) for x in result]))
        self.assertEqual(result.clauses[0]._params[0].pprint(), "a.b.c")
        self.assertEqual(result.clauses[1]._params[0].pprint(), "~a!b.d")
        self.assertEqual(result.clauses[2]._params[0].pprint(), "$x")
        self.assertEqual(result.clauses[3]._params[0].pprint(), "$x.a.b")

    def test_action_binding_expansion(self):
        bindings = {"x" : FP.parseString('a.b.c')[0] }
        parsed_action = AP.parseString("ActionAdd($x)")
        bound_action = parsed_action.bind(bindings)
        self.assertIsInstance(bound_action, action.Action)
        self.assertEqual(bound_action.pprint(), "ActionAdd(a.b.c)")

    def test_action_definition(self):
        test_str = "test: (::α)\nActionAdd(a.b.c)\n\nend"
        definition = AP.action_definition.parseString(test_str)
        self.assertEqual(definition[0][-1].name, "test")


if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Action_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
