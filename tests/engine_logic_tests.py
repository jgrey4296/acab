import unittest
import logging
from test_context import pyRule
import pyRule.trie as T
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir

import IPython

class Engine_Logic_Tests(unittest.TestCase):

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
        self.e.load_file(join(".", "testfiles", "exampleRule1.trie"))
        self.assertTrue(self.e.query(".a.b.c?, ~.d.e.f?"))
        self.assertEqual(len(self.e._rules), 1)
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.c?, .d.e.f?"))
          
    def test_multi_rule_load_from_single_file(self):
        self.e.load_file(join(".", "testfiles", "exampleRules2.trie"))
        self.assertEqual(len(self.e._rules), 2)
        self.assertTrue(self.e.query(".a.b.c?, .a.other!ex?, ~.d.e.f?, ~.d.e.g?"))
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.c?, .a.other!ex?, .d.e.f?, .d.e.g?"))

    def test_file_load_with_comments(self):
        self.e.load_file(join(".", "testfiles", "exampleComments.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query("~.this.should.not.assert?"))

    def test_file_load_retraction(self):
        self.e.load_file(join(".", "testfiles", "retractionTest.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query(".a.b.c?"))
        self.e._run_rules()
        self.assertTrue(self.e.query("~.a.b.c?"))

    def test_file_load_multi_clauses(self):
        self.e.load_file(join(".", "testfiles", "multiclause_rule.trie"))
        self.assertEqual(len(self.e._rules), 1)
        self.assertTrue(self.e.query(".a.b.c?, .a.c!d?"))
        self.e._run_rules()
        self.assertTrue(self.e.query(".a.b.c?, .a.c!d?, .a.b.e?"))
        
    def test_file_load_string_query(self):
        self.assertTrue(False)

    def test_file_load_string_assert(self):
        self.assertTrue(False)

    def test_file_load_string_retract(self):
        self.assertTrue(False)

    def test_file_load_transform(self):
        self.assertTrue(False)

    def test_file_load_multi_transform(self):
        self.assertTrue(False)

    def test_file_exclusion_query(self):
        self.assertTrue(False)

    def test_file_exclusion_update(self):
        self.assertTrue(False)

    def test_file_exclusion_override(self):
        self.assertTrue(False)

    

        

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
