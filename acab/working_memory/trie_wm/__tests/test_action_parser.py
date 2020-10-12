import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.core.sentence import Sentence
from acab.abstract.rule import action
from acab.abstract.rule.production_operator import ProductionOperator
from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import FactParser as FP

def S(*words):
    return Sentence.build(words)

class Trie_Action_Parser_Tests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_string_value(self):
        result = AP.parseString('λS.ActionAdd "blah bloo" "blee" "awef"')
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result, action.Action)
        self.assertEqual(result.clauses[0].op.pprint(), "S.ActionAdd")
        self.assertEqual([x._value for x in result.clauses[0]._params], ["blah bloo","blee","awef"])

    def test_actions_fact_str(self):
        result = AP.parseString('λS.ActionAdd a.b.c, λoperator.action.add ~a!b.d, λoperator.action.add $x, λoperator.action.add $x.a.b')
        self.assertIsInstance(result, action.Action)
        self.assertEqual(len(result), 4)
        self.assertTrue(all([isinstance(x, action.ActionComponent) for x in result]))
        self.assertEqual(result.clauses[0]._params[0].pprint(), "a.b.c")
        self.assertEqual(result.clauses[1]._params[0].pprint(), "~a!b.d")
        self.assertEqual(result.clauses[2]._params[0].pprint(), "$x")
        self.assertEqual(result.clauses[3]._params[0].pprint(), "$x.a.b")

    def test_action_binding_expansion(self):
        bindings = {"x" : FP.parseString('a.b.c')[0] }
        parsed_action = AP.parseString("λoperator.action.add $x")
        bound_action = parsed_action.bind(bindings)
        self.assertIsInstance(bound_action, action.Action)
        self.assertEqual(bound_action.pprint().strip(), r"λoperator.action.add a.b.c")

    def test_action_definition(self):
        test_str = "test: (::α)\nλoperator.action.add a.b.c\n\nend"
        definition = AP.action_definition.parseString(test_str)
        self.assertEqual(definition[0][-1].name, "test")

    def test_parse_action_no_params(self):
        test_str = "λoperator.action.add"

        result = AP.action_component.parseString(test_str)[0]
        self.assertIsInstance(result, action.ActionComponent)

    # TODO test action sugar, test sentences, test values, test multi params
    @unittest.skip("TODO")
    def test_action_sugar(self):
        return


if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Action_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
