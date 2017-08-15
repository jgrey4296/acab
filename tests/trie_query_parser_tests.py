import unittest
import logging
from test_context import pyRule
import pyRule.trie as T
import pyRule.trie.QueryParser as QP
from pyRule.trie import Query 
from pyRule.trie.Clause import Clause
from pyRule.Comparisons import Comparison, COMP
import pyRule.utils as util

class Trie_Query_Parser_Tests(unittest.TestCase):
      
    def setUp(self):
        return 1

    def tearDown(self):
        return 1
        
    #----------
    #use testcase snippets
    def test_basic_comp_internal(self):
        result = QP.COMP_Internal.parseString('>20')[0]
        self.assertIsInstance(result, Comparison)
        
    def test_basic_comparison(self):
        result = QP.comparison.parseString('(>20, <40, !=$x, ==$y)')[:]
        self.assertEqual(len(result), 4)
        self.assertTrue(all([isinstance(x, Comparison) for x in result]))
        self.assertEqual(result[0].op, COMP.GT)
        self.assertEqual(result[1].op, COMP.LT)
        self.assertEqual(result[2].op, COMP.NE)
        self.assertEqual(result[3].op, COMP.EQ)

    def test_basic_query_core(self):
        result = QP.QueryCore.parseString('.a(>20)')[0]
        self.assertEqual(len(result.get_meta_eval(util.META_OP.COMP)), 1)
        self.assertIsInstance(result.get_meta_eval(util.META_OP.COMP)[0], Comparison)

    def test_basic_query_core_multi_comparison(self):
        result = QP.QueryCore.parseString('.a(>20, <30)')[0]
        self.assertEqual(len(result.get_meta_eval(util.META_OP.COMP)), 2)
        self.assertTrue(all([isinstance(x, Comparison) for x in result.get_meta_eval(util.META_OP.COMP)]))

    def _test_basic_query_core_with_exclusion(self):
        self.assertTrue(True)

    def _test_basic_clause(self):
        self.assertTrue(True)

    def _test_basic_negated_clause(self):
        self.assertTrue(True)

    def _test_basic_multi_clause(self):
        self.assertTrue(True)

    def _test_basic_query_construction(self):
        self.assertTrue(True)
        

if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Query_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
