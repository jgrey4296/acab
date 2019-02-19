import unittest
import logging
from test_context import pyRule
from pyRule.trie import ActionParser as AP
from pyRule.trie import FactParser as FP
from pyRule import Actions

class Trie_Action_Parser_Tests(unittest.TestCase):

      def setUp(self):
            return 1

      def tearDown(self):
            return 1

      #----------
      #use testcase snippets
      def test_simple_action_parse(self):
            result = AP.parseString("+(20, 30, 40)")
            self.assertEqual(len(result), 1)
            self.assertTrue(all([isinstance(x, Actions.Action) for x in result]))
            self.assertEqual(result[0]._op, Actions.ACTS.ADD)
            self.assertEqual(result[0]._values, [20, 30, 40])

      def test_custom_action_parse(self):
            result = AP.parseString("blah(20, 30, 40)")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Actions.Action)
            self.assertEqual(result[0]._op, "blah")
            self.assertEqual(result[0]._values, [20, 30, 40])

      def test_string_value(self):
            result = AP.parseString('+("blah bloo", "blee", "awef")')
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Actions.Action)
            self.assertEqual(result[0]._op, Actions.ACTS.ADD)
            self.assertEqual(result[0]._values, ["blah bloo","blee","awef"])

      def test_actions_parse(self):
            result = AP.parseString('+(2), -(3), @(4), blah(5)')
            self.assertEqual(len(result), 4)
            self.assertTrue(all([isinstance(x, Actions.Action) for x in result]))
            for action, op in zip(result, [Actions.ACTS.ADD,
                                           Actions.ACTS.RETRACT,
                                           Actions.ACTS.PRINT,
                                           "blah"]):
                  self.assertEqual(action._op, op)

      def test_actions_fact_str(self):
            result = AP.parseString('+(.a.b.c), -(.a!b.d), +($x), +($x.a.b)')
            self.assertEqual(len(result), 4)
            self.assertTrue(all([isinstance(x, Actions.Action) for x in result]))

      def test_action_str_equal(self):
            actions = ["+(2)", "-(3)", "@(4)", "blah(5)"]
            parsed = [AP.parseString(x)[0] for x in actions]
            zipped = zip(actions, parsed)
            self.assertTrue(all([x == str(y) for x,y in zipped]))

      def test_action_binding_expansion(self):
            bindings = {"x" : FP.parseString('.a.b.c')[0] }
            action = AP.parseString("+($x)")[0]
            newAction = action.expandBindings(bindings)
            self.assertEqual(str(newAction), "+(.a.b.c)")



if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Action_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
