import unittest
import logging
from test_context import pyRule
import pyRule.trie as T
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir

import IPython

class Engine_Logic_Tests(unittest.TestCase):

    def path(self, filename):
        return join('.', 'testfiles', filename)
    
    def setUp(self):
        self.e = T.Engine()

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_simple_logic(self):
        self.e.registerRules(""".a.test.rule:
        	.a.b.c?
        
        	+(.a.b.d)
        end""")
        self.e.add(".a.b.c")
        self.assertTrue(self.e.query("~.a.b.d?"))
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.d?"))

    def test_simple_file_load(self):
        self.e.load_file(self.path("exampleRule1.trie"))
        self.assertTrue(self.e.query(".a.b.c?, ~.d.e.f?"))
        self.assertEqual(len(self.e._rules), 1)
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.c?, .d.e.f?"))
          
    def test_multi_rule_load_from_single_file(self):
        self.e.load_file(self.path("exampleRules2.trie"))
        self.assertEqual(len(self.e._rules), 2)
        self.assertTrue(self.e.query(".a.b.c?, .a.other!ex?, ~.d.e.f?, ~.d.e.g?"))
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.c?, .a.other!ex?, .d.e.f?, .d.e.g?"))

    def test_file_load_with_comments(self):
        self.e.load_file(self.path( "exampleComments.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query("~.this.should.not.assert?"))

    def test_file_load_retraction(self):
        self.e.load_file(self.path( "retractionTest.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query(".a.b.c?"))
        self.e._run_rules()
        self.assertTrue(self.e.query("~.a.b.c?"))

    def test_file_load_multi_clauses(self):
        self.e.load_file(self.path( "multiclause_rule.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query(".a.b.c?, .a.c!d?"))
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.c?, .a.c!d?, .a.b.e?"))
        
    def test_file_load_string_query(self):
        self.e.load_file(self.path("string_query_test.trie"))
        self.assertEqual(len(self.e._rules), 2)
        self.assertTrue(self.e.query("~.a.b.e?"))
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.e?, .a.b.f?"))

    def test_file_load_string_assert(self):
        self.e.load_file(self.path("string_assert_test.trie"))
        self.assertTrue(self.e.query('~.a.b."an asserted string"?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('.a.b."an asserted string"?'))
        
    def test_file_load_string_retract(self):
        self.e.load_file(self.path("string_retract_test.trie"))
        self.assertTrue(self.e.query('.a.b."a test string"?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('~.a.b."a test string"?'))
        
    def test_file_load_transform(self):
        self.e.load_file(self.path("transform_test.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query('.a.b!5?, .a.c!10?, .a.d!20?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('.a.b!25?'))
        self.assertTrue(self.e.query('.a.c!30?'))
        self.assertTrue(self.e.query('.a.d!40?'))

    def test_transform_selection_single(self):
        self.e.load_file(self.path("transform_selection_single.trie"))
        self.assertTrue('.a.b!5?, .a.c!10?, .a.d!20?')
        self.e._run_rules()
        queried = [True for x in [".a.b!25?",".a.c!30?",".a.d!40?"] if bool(self.e.query(x))]
        self.assertEqual(len(queried), 1)

        
    def test_file_load_multi_transform(self):
        self.e.load_file(self.path("multi_transform_test.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query('.a.b!5?, .a.c!10?, .a.d!20?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('.a.b!50?'))
        self.assertTrue(self.e.query('.a.c!60?'))
        self.assertTrue(self.e.query('.a.d!80?'))

    def test_file_exclusion_update(self):
        self.e.load_file(self.path("exclusion_update_test.trie"))
        self.assertTrue(self.e.query('.a.b!20?, ~.a.b.!10?, ~.a.b!5?'))

    def test_macro_binding(self):
        self.e.load_file(self.path("macro_binding_test.trie"))
        self.assertTrue(self.e.query('.a.b.c.d?, .a.b.c.e?'))

    def test_macro_binding_empty(self):
        self.e.load_file(self.path("macro_empty_binding_test.trie"))
        self.assertTrue(self.e.query('~.a.b.c?'))

    def test_action_macro(self):
        self.e.load_file(self.path("action_macro.trie"))
        self.assertTrue(self.e.query('.a.b.first?, .a.c.second?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('~.a.b.first?, ~.a.c.second?, .a.b.second?, .a.c.first?'))
        rule = list(self.e._rules.values())[0]
        self.assertEqual(len(rule._actions), 4)

    def test_two_action_macros(self):
        self.e.load_file(self.path("two_action_macro.trie"))
        self.assertTrue(self.e.query('.a.b.first?, .a.c.second?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('~.a.b.first?, ~.a.c.second?, .a.b.second?, .a.c.first?, .blah?, .bloo?'))
        rule = list(self.e._rules.values())[0]
        self.assertEqual(len(rule._actions), 6)

    def test_action_macros_with_values(self):
        self.e.load_file(self.path("valued_action_macro.trie"))
        self.assertTrue(self.e.query('.a.b.c?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('.a.b.c?, .a.b."this is a test"?'))

    def test_variable_macro_in_action_macro_test(self):
        self.e.load_file(self.path("variable_macro_in_action_macro_test.trie"))
        self.assertTrue(self.e.query('.a.b.c?'))
        self.e._run_rules()
        self.assertTrue(self.e.query('.a.b.c?, .a.b.blah?'))
        

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
