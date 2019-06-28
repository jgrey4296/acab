import unittest
import logging
from test_context import py_rule
import py_rule.trie as T
import py_rule.trie.parsing.QueryParser as QP
from py_rule.abstract.query import Query
from py_rule.abstract.clause import Clause
from py_rule.abstract.comparisons import Comparison, COMP, CompOp
import py_rule.utils as util
from py_rule.utils import EXOP
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
        result = QP.constraints.parseString('>20, <40, !=$x, ==$y, ~=/blah/')[0]
        self.assertEqual(result[0], "constraints")
        self.assertEqual(len(result[1]), 5)
        self.assertTrue(all([isinstance(x, Comparison) for x in result[1]]))
        self.assertIsInstance(result[1][0]._op, type(CompOp.op_list['>']))
        self.assertIsInstance(result[1][1]._op, type(CompOp.op_list['<']))
        self.assertIsInstance(result[1][2]._op, type(CompOp.op_list['!=']))
        self.assertIsInstance(result[1][3]._op, type(CompOp.op_list['==']))
        self.assertIsInstance(result[1][4]._op, type(CompOp.op_list['~=']))

    def test_basic_regex_comparison(self):
        result = QP.COMP_Internal.parseString('~= /blah/')[0]
        self.assertIsInstance(result, Comparison)
        self.assertIsInstance(result._op, type(CompOp.op_list['~=']))
        self.assertEqual(result._value, 'blah')


    def test_basic_query_core(self):
        result = QP.QueryCore.parseString('a(>20).')[0]
        self.assertTrue('constraints' in result._data)
        self.assertEqual(len(result._data['constraints']), 1)
        self.assertIsInstance(result._data['constraints'][0], Comparison)

    def test_basic_query_core_multi_comparison(self):
        result = QP.QueryCore.parseString('a(>20, <30).')[0]
        self.assertEqual(len(result._data['constraints']), 2)
        self.assertTrue(all([isinstance(x, Comparison) for x in result._data['constraints']]))

    def test_basic_query_core_with_exclusion(self):
        result = QP.QueryCore.parseString('a(>20)!')[0]
        self.assertEqual(result._data['op'], EXOP.EX)

    def test_basic_clause(self):
        result = QP.clause.parseString('a.b.c?')[0]
        self.assertIsInstance(result, Clause)
        self.assertEqual(len(result._sentence), 3)
        self.assertEqual(result._sentence[-1]._value, 'c')
        self.assertEqual(result._sentence[-1]._data['op'], EXOP.DOT)

    def test_basic_clause_with_bind(self):
        result = QP.clause.parseString('a.b.$c?')[0]
        self.assertIsInstance(result, Clause)
        self.assertEqual(len(result._sentence), 3)
        self.assertEqual(result._sentence[-1]._value, 'c')
        self.assertEqual(result._sentence[-1]._data['op'], EXOP.DOT)
        self.assertTrue(result._sentence[-1]._data['bind'])

    def test_basic_negated_clause(self):
        result = QP.clause.parseString('~a.b.c?')[0]
        self.assertIsInstance(result, Clause)
        self.assertTrue(result._negated)


    def test_basic_multi_clause(self):
        result = QP.clauses.parseString('a.b.c?, a.b.d?, a.b.e?')[0]
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result._clauses), 3)
        self.assertTrue(all([isinstance(x, Clause) for x in result._clauses]))
        self.assertEqual(result._clauses[0]._sentence[-1]._value, 'c')
        self.assertEqual(result._clauses[1]._sentence[-1]._value, 'd')
        self.assertEqual(result._clauses[2]._sentence[-1]._value, 'e')

    def test_basic_multi_clause_mixed_negation(self):
        result = QP.clauses.parseString('a.b.c?, ~a.b.d?, a.b.e?, ~a.b.f?')[0]
        self.assertIsInstance(result, Query)
        self.assertTrue(all([isinstance(x, Clause) for x in result._clauses]))
        self.assertFalse(result._clauses[0]._negated)
        self.assertTrue(result._clauses[1]._negated)
        self.assertFalse(result._clauses[2]._negated)
        self.assertTrue(result._clauses[3]._negated)


    def test_basic_query_construction(self):
        result = QP.parseString('a.b.c?, a.b.d?, a.b.e?')
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result._clauses), 3)

    def test_clause_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2')[0]
        self.assertIsInstance(result, Clause)
        self.assertIsNotNone(result._fallback)
        self.assertEqual(len(result._fallback), 1)

        self.assertEqual(result._fallback[0][0], 'x')
        self.assertEqual(result._fallback[0][1][-1]._value, 2)

    def test_clause_negated_fallback(self):
        with self.assertRaises(Exception):
            QP.clause.parseString('~a.b.c? || $x:2')

    def test_clause_multi_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2, $y:5')[0]
        self.assertIsInstance(result, Clause)
        self.assertIsNotNone(result._fallback)
        self.assertEqual(len(result._fallback), 2)
        self.assertEqual(result._fallback[0][0], 'x')
        self.assertEqual(result._fallback[0][1][-1]._value, 2)
        self.assertEqual(result._fallback[1][0], 'y')
        self.assertEqual(result._fallback[1][1][-1]._value, 5)

    def test_clause_fallback_strings(self):
        result = QP.clause.parseString('a.b.c? || $x:a.b!c, $y:b.d.e')[0]
        self.assertIsInstance(result, Clause)
        self.assertIsNotNone(result._fallback)
        self.assertEqual(len(result._fallback), 2)
        self.assertEqual(result._fallback[0][0], 'x')
        self.assertEqual(result._fallback[0][1][-1]._value, 'c')
        self.assertEqual(result._fallback[1][0], 'y')
        self.assertEqual(result._fallback[1][1][-1]._value, 'e')


    # def test_rulebind_parsing(self):
    #     result = QP.clause.parseString('a.b.c(^$x)?')[0]
    #     self.assertIsInstance(result, Clause)
    #     IPython.embed(simple_prompt=True)

    #     # self.assertIsInstance(result._sentence[-1]


    def test_fact_str_equal(self):
        queries = ["a.b.c?", "a.b!c?", 'a.b."a string".c?',
                   'a.b!"a string"!c?', 'a.b(> 20)?',
                   'a.$b?', 'a!$b?', 'a.$b(> $c)?',
                   'a.$b(> 20, < 40, != $x, == $y)?',
                   '~a.b.c?', '~a!b.c?',
                   'a.$b(~= /blah/)?',
                   'a.b.c? || $x:2',
                   'a.b.d? || $x:5, $y:blah']
                   # 'a.b.c(^$x)?']
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
