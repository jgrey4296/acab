import unittest
import logging
from test_context import py_rule
from py_rule.knowledge_bases.trie_kb.parsing import ActionParser as AP
from py_rule.knowledge_bases.trie_kb.parsing import FactParser as FP
from py_rule.modules.standard_operators.operator_module import OperatorSpec
from py_rule.abstract import action

class ActionBlah(action.ActionOp):
    def __init__(self):
        super().__init__("blah")

    def __call__(self, engine, params):
        logging.info("Blah")

ActionBlah()

class Trie_Action_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        os = OperatorSpec()
        os._construct_action_ops()
        AP.build_operators()

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_simple_action_parse(self):
        result = AP.parseString("+(20, 30, 40)")[0]
        self.assertIsInstance(result, action.Action)
        self.assertEqual(result._op._op_str, '+')
        self.assertEqual([x[-1]._value for x in result._values], [20, 30, 40])

    def test_custom_action_parse(self):
        result = AP.parseString("blah(20, 30, 40)")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0], action.Action)
        self.assertEqual(result[0]._op._op_str, "blah")
        self.assertEqual([x[-1]._value for x in result[0]._values], [20, 30, 40])

    def test_string_value(self):
        result = AP.parseString('+("blah bloo", "blee", "awef")')
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0], action.Action)
        self.assertEqual(result[0]._op._op_str, "+")
        self.assertEqual([x[-1]._value for x in result[0]._values], ["blah bloo","blee","awef"])

    def test_actions_parse(self):
        result = AP.parseString('+(2), -(3), @(4)')
        self.assertEqual(len(result), 3)
        self.assertTrue(all([isinstance(x, action.Action) for x in result]))
        for parsed_action, op in zip(result, ["+", "-", "@"]):
            self.assertEqual(parsed_action._op._op_str, op)

    def test_actions_fact_str(self):
        result = AP.parseString('+(a.b.c), -(a!b.d), +($x), +($x.a.b)')
        self.assertEqual(len(result), 4)
        self.assertTrue(all([isinstance(x, action.Action) for x in result]))

    def test_action_str_equal(self):
        actions = ["+(2)", "-(3)", "@(4)"]
        parsed = [AP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        self.assertTrue(all([x == str(y) for x,y in zipped]))

    def test_action_binding_expansion(self):
        bindings = {"x" : FP.parseString('a.b.c')[0] }
        action = AP.parseString("+($x)")[0]
        newAction = action.expand_bindings(bindings)
        self.assertEqual(str(newAction), "+(a.b.c)")

    def test_action_macro_definition_empty(self):
        test_str = "#test():\n +(a.b.c)\nend"
        definition = AP.action_definition.parseString(test_str)[0]
        self.assertEqual(definition._name, "test")


if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Action_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
