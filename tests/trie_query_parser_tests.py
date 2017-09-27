import unittest
import logging
from test_context import pyRule
import pyRule.trie as T
import pyRule.trie.QueryParser as QP
from pyRule import Query 
from pyRule import Clause
from pyRule.Comparisons import Comparison, COMP
import pyRule.utils as util
from pyRule.utils import EXOP
import IPython

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
        result = QP.comparison.parseString('(>20, <40, !=$x, ==$y, ~=/blah/)')[:]
        self.assertEqual(len(result), 5)
        self.assertTrue(all([isinstance(x, Comparison) for x in result]))
        self.assertEqual(result[0].op, COMP.GT)
        self.assertEqual(result[1].op, COMP.LT)
        self.assertEqual(result[2].op, COMP.NE)
        self.assertEqual(result[3].op, COMP.EQ)
        self.assertEqual(result[4].op, COMP.RE)

    def test_basic_regex_comparison(self):
        result = QP.COMP_Internal.parseString('~= /blah/')[0]
        self.assertIsInstance(result, Comparison)
        self.assertEqual(result.op, COMP.RE)
        self.assertEqual(result.value, 'blah')
        self.assertIsNone(result.bind)
        
        
    def test_basic_query_core(self):
        result = QP.QueryCore.parseString('.a(>20)')[0]
        self.assertEqual(len(result.get_meta_eval(util.META_OP.COMP)), 1)
        self.assertIsInstance(result.get_meta_eval(util.META_OP.COMP)[0], Comparison)

    def test_basic_query_core_multi_comparison(self):
        result = QP.QueryCore.parseString('.a(>20, <30)')[0]
        self.assertEqual(len(result.get_meta_eval(util.META_OP.COMP)), 2)
        self.assertTrue(all([isinstance(x, Comparison) for x in result.get_meta_eval(util.META_OP.COMP)]))

    def test_basic_query_core_with_exclusion(self):
        result = QP.QueryCore.parseString('!a')[0]
        self.assertEqual(result._op, EXOP.EX)

    def test_basic_clause(self):
        result = QP.clause.parseString('.a.b.c?')[0]
        self.assertIsInstance(result, Clause)
        self.assertEqual(len(result.components), 3)
        self.assertEqual(result.components[-1]._value, 'c')
        self.assertEqual(result.components[-1]._op, EXOP.DOT)

    def test_basic_clause_with_bind(self):
        result = QP.clause.parseString('.a.b.$c?')[0]
        self.assertIsInstance(result, Clause)
        self.assertEqual(len(result.components), 3)
        self.assertEqual(result.components[-1]._value, 'c')
        self.assertEqual(result.components[-1]._op, EXOP.DOT)
        self.assertTrue(result.components[-1]._meta_eval[util.META_OP.BIND])
    
    def test_basic_negated_clause(self):
        result = QP.clause.parseString('~.a.b.c?')[0]
        self.assertIsInstance(result, Clause)
        self.assertTrue(result.negated)


    def test_basic_multi_clause(self):
        result = QP.clauses.parseString('.a.b.c?, .a.b.d?, .a.b.e?')[0]
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result._clauses), 3)
        self.assertTrue(all([isinstance(x, Clause) for x in result._clauses]))
        self.assertEqual(result._clauses[0].components[-1]._value, 'c')
        self.assertEqual(result._clauses[1].components[-1]._value, 'd')
        self.assertEqual(result._clauses[2].components[-1]._value, 'e')
        
    def test_basic_multi_clause_mixed_negation(self):
        result = QP.clauses.parseString('.a.b.c?, ~.a.b.d?, .a.b.e?, ~.a.b.f?')[0]
        self.assertIsInstance(result, Query)
        self.assertTrue(all([isinstance(x, Clause) for x in result._clauses]))
        self.assertFalse(result._clauses[0].negated)
        self.assertTrue(result._clauses[1].negated)
        self.assertFalse(result._clauses[2].negated)
        self.assertTrue(result._clauses[3].negated)
                

    def test_basic_query_construction(self):
        result = QP.parseString('.a.b.c?, .a.b.d?, .a.b.e?')
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result._clauses), 3)

    def test_clause_fallback(self):
        result = QP.clause.parseString('.a.b.c? || $x:2')[0]
        self.assertIsInstance(result, Clause)
        self.assertIsNotNone(result.fallback)
        self.assertEqual(len(result.fallback), 1)
        self.assertEqual(result.fallback[0][0].value, 'x')
        self.assertEqual(result.fallback[0][1], 2)

    def test_clause_negated_fallback(self):
        with self.assertRaises(Exception):
            QP.clause.parseString('~.a.b.c? || $x:2')

    def test_clause_multi_fallback(self):
        result = QP.clause.parseString('.a.b.c? || $x:2, $y:5')[0]
        self.assertIsInstance(result, Clause)
        self.assertIsNotNone(result.fallback)
        self.assertEqual(len(result.fallback), 2)
        self.assertEqual(result.fallback[0][0].value, 'x')
        self.assertEqual(result.fallback[0][1], 2)
        self.assertEqual(result.fallback[1][0].value, 'y')
        self.assertEqual(result.fallback[1][1], 5)

    def test_rulebind_parsing(self):
        result = QP.clause.parseString('.a.b.c(^$x)?')[0]
        self.assertIsInstance(result, Clause)
        self.assertIsInstance(result.components[-1].get_meta_eval(util.META_OP.RULEBIND), util.Bind)

    
        
    def test_fact_str_equal(self):
        queries = [".a.b.c?", ".a.b!c?", '.a.b."a string".c?',
                   '.a.b!"a string"!c?', '.a.b(> 20)?',
                   '.a.$b?', '.a!$b?', '.a.$b(> $c)?',
                   '.a.$b(> 20, < 40, != $x, == $y)?',
                   '~.a.b.c?', '~.a!b.c?',
                   '.a.$b(~= /blah/)?',
                   '.a.b.c? || $x:2',
                   '.a.b.d? || $x:5, $y:blah',
                   '.a.b.c(^$x)?']
        parsed = [QP.parseString(x) for x in queries]
        zipped = zip(queries, parsed)
        for a,q in zipped:
            self.assertEqual(a,str(q))

        

if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Query_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
