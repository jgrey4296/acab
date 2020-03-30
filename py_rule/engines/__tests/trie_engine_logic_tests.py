import unittest
import logging
import py_rule.abstract.trie as T
from py_rule.engines.trie_engine import TrieEngine
from py_rule.modules.operators.operator_module import OperatorSpec
from py_rule.abstract.rule import Rule
from os.path import join, isfile, exists, isdir
from os.path import split, splitext, expanduser, abspath
from os import listdir


class Engine_Logic_Tests(unittest.TestCase):
    os = None

    @classmethod
    def setUpClass(cls):
        Engine_Logic_Tests.os = OperatorSpec()
        Engine_Logic_Tests.os.construct_operators()

    def path(self, filename):
        """ Navigate from the file,
        not the cwd """
        return abspath(join("testfiles", filename))

    def setUp(self):
        self.e = TrieEngine(modules=[Engine_Logic_Tests.os])

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_simple_logic(self):
        self.e.add("""ρ::a.test.rule:\na.b.c?\n\nActionAdd(a.b.d)\n\nend""")
        rule = self.e.query('a.test.$x?')[0]['x']
        self.assertIsInstance(rule, Rule)
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("~a.b.d?"))
        self.assertFalse(self.e._proposed_actions)
        self.e._run_rule(rule)
        self.assertTrue(self.e._proposed_actions)
        data_rule = self.e._proposed_actions[0]
        self.e._perform_actions(data_rule[0], data_rule[1]._action)
        self.assertTrue(self.e.query("a.b.d?"))

    def test_simple_file_load(self):
        self.e.load_file(self.path("exampleRule1.trie"))
        self.assertTrue(self.e.query("a.b.c?, ~d.e.f?"))
        self.assertEqual(len(self.e._rules), 1)
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query("a.b.c?, d.e.f?"))

    def test_multi_rule_load_from_single_file(self):
        self.e.load_file(self.path("exampleRules2.trie"))
        self.assertEqual(len(self.e._rules), 2)
        self.assertTrue(self.e.query("a.b.c?, a.other!ex?, ~d.e.f?, ~d.e.g?"))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query("a.b.c?, a.other!ex?, d.e.f?, d.e.g?"))

    def test_file_load_with_comments(self):
        self.e.load_file(self.path( "exampleComments.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query("~this.should.not.assert?"))

    def test_file_load_retraction(self):
        self.e.load_file(self.path( "retractionTest.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query("a.b.c?"))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query("~a.b.c?"))

    def test_file_load_multi_clauses(self):
        self.e.load_file(self.path( "multiclause_rule.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query("a.b.c?, a.c!d?"))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query("a.b.c?, a.c!d?, a.b.e?"))

    def test_file_load_string_query(self):
        self.e.load_file(self.path("string_query_test.trie"))
        self.assertEqual(len(self.e._rules), 2)
        self.assertTrue(self.e.query("~a.b.e?"))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query("a.b.e?, a.b.f?"))

    def test_file_load_string_assert(self):
        self.e.load_file(self.path("string_assert_test.trie"))
        self.assertTrue(self.e.query('~a.b."an asserted string"?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.b."an asserted string"?'))

    def test_file_load_string_retract(self):
        self.e.load_file(self.path("string_retract_test.trie"))
        self.assertTrue(self.e.query('a.b."a test string"?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('~a.b."a test string"?'))

    def test_file_load_transform(self):
        self.e.load_file(self.path("transform_test.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query('a.b!5?, a.c!10?, a.d!20?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.b!25?'))
        self.assertTrue(self.e.query('a.c!30?'))
        self.assertTrue(self.e.query('a.d!40?'))

    def test_transform_selection_single(self):
        self.e.load_file(self.path("transform_selection_single.trie"))
        self.assertTrue('a.b!5?, a.c!10?, a.d!20?')
        # query rule
        # _run_rule
        # _perform_actions
        queried = [True for x in ["a.b!25?","a.c!30?","a.d!40?"] if bool(self.e.query(x))]
        self.assertEqual(len(queried), 3)


    def test_file_load_multi_transform(self):
        self.e.load_file(self.path("multi_transform_test.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query('a.b!5?, a.c!10?, a.d!20?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.b!50?'))
        self.assertTrue(self.e.query('a.c!60?'))
        self.assertTrue(self.e.query('a.d!80?'))

    def test_file_exclusion_update(self):
        self.e.load_file(self.path("exclusion_update_test.trie"))
        self.assertTrue(self.e.query('a.b!20?, ~a.b.!10?, ~a.b!5?'))

    def test_macro_binding(self):
        self.e.load_file(self.path("macro_binding_test.trie"))
        self.assertTrue(self.e.query('a.b.c.d?, a.b.c.e?'))

    def test_macro_binding_empty(self):
        self.e.load_file(self.path("macro_empty_binding_test.trie"))
        self.assertTrue(self.e.query('~a.b.c?'))

    def test_action_macro(self):
        self.e.load_file(self.path("action_macro.trie"))
        self.assertTrue(self.e.query('a.b.first?, a.c.second?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('~a.b.first?, ~a.c.second?, a.b.second?, a.c.first?'))
        rule = list(self.e._rules.values())[0]
        self.assertEqual(len(rule._actions), 4)

    def test_two_action_macros(self):
        self.e.load_file(self.path("two_action_macro.trie"))
        self.assertTrue(self.e.query('a.b.first?, a.c.second?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('~a.b.first?, ~a.c.second?, a.b.second?, a.c.first?, .blah?, .bloo?'))
        rule = list(self.e._rules.values())[0]
        self.assertEqual(len(rule._actions), 6)

    def test_action_macros_with_values(self):
        self.e.load_file(self.path("valued_action_macro.trie"))
        self.assertTrue(self.e.query('a.b.c?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.b.c?, a.b."this is a test"?'))

    def test_variable_macro_in_action_macro_test(self):
        self.e.load_file(self.path("variable_macro_in_action_macro_test.trie"))
        self.assertTrue(self.e.query('a.b.c?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.b.c?, a.b.blah?'))

    def test_rule_selection_running(self):
        self.e.load_file(self.path("selection_of_rules_test.trie"))
        self.assertTrue(self.e.query('a.b.c?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.b.c?, ~blah?, bloo?'))
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.b.c?, blah?, bloo?'))

    def rule_load_with_comments(self):
        self.e.load_file(self.path("rule_with_comments.trie"))
        self.assertEqual(len(self.e._rules), 1)
        rule = self.e._rules['a.rule']
        self.assertEqual(len(rule._tags), 2)
        self.assertEqual(len(rule._query), 2)
        self.assertEqual(len(rule._transform), 2)
        self.assertEqual(len(rule._actions), 2)
        # query rule
        # _run_rule
        # _perform_actions
        self.assertTrue(self.e.query('a.d.c?, a.d.d?, a.b!30?'))


if __name__ == "__main__":
    #use python $filename to use this logging setup
      LOGLEVEL = logging.INFO
      logFileName = "log.Engine_Logic_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
