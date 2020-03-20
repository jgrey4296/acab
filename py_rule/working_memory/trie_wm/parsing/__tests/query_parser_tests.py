import unittest
import logging
import py_rule.working_memory.trie_wm.parsing.QueryParser as QP
from py_rule.abstract.query import Query
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.comparison import Comparison, CompOp
from py_rule.modules.operators.operator_module import OperatorSpec
from py_rule.working_memory.trie_wm import util as KBU
from py_rule.util import CONSTRAINT_S, VALUE_TYPE_S, REGEX_S

class Trie_Query_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        os = OperatorSpec()
        os._construct_comp_ops()
        QP.build_operators()

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets

    def test_basic_regex_comparison(self):
        result = QP.COMP_Internal.parseString('RegMatch /blah/')[0]
        self.assertIsInstance(result, Comparison)
        self.assertEqual(result._op, 'RegMatch')
        self.assertEqual(result._value._value, 'blah')
        self.assertEqual(result._value._data[VALUE_TYPE_S], REGEX_S)

    def test_basic_clause(self):
        result = QP.clause.parseString('a.b.c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[-1]._value, 'c')
        self.assertEqual(result[-1]._data[KBU.OPERATOR_S], KBU.EXOP.DOT)

    def test_basic_clause_with_bind(self):
        result = QP.clause.parseString('a.b.$c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[-1]._value, 'c')
        self.assertEqual(result[-1]._data[KBU.OPERATOR_S], KBU.EXOP.DOT)
        self.assertTrue(result[-1]._data[KBU.BIND_S])

    def test_basic_negated_clause(self):
        result = QP.clause.parseString('~a.b.c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(result._negated)

    def test_basic_multi_clause(self):
        result = QP.clauses.parseString('a.b.c?, a.b.d?, a.b.e?')[0]
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result._clauses), 3)
        self.assertTrue(all([isinstance(x, Sentence) for x in result._clauses]))
        self.assertEqual(result._clauses[0][-1]._value, 'c')
        self.assertEqual(result._clauses[1][-1]._value, 'd')
        self.assertEqual(result._clauses[2][-1]._value, 'e')

    def test_basic_multi_clause_mixed_negation(self):
        result = QP.clauses.parseString('a.b.c?, ~a.b.d?, a.b.e?, ~a.b.f?')[0]
        self.assertIsInstance(result, Query)
        self.assertTrue(all([isinstance(x, Sentence) for x in result._clauses]))
        self.assertFalse(result._clauses[0]._negated)
        self.assertTrue(result._clauses[1]._negated)
        self.assertFalse(result._clauses[2]._negated)
        self.assertTrue(result._clauses[3]._negated)

    def test_basic_query_construction(self):
        result = QP.parseString('a.b.c?, a.b.d?, a.b.e?')
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result._clauses), 3)

    def test_clause_fallback_strings(self):
        result = QP.clause.parseString('a.b.c? || $x:a.b!c, $y:b.d.e')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result._fallback)
        self.assertEqual(len(result._fallback), 2)
        self.assertEqual(result._fallback[0][0], 'x')
        self.assertEqual(result._fallback[0][1][-1]._value, 'c')
        self.assertEqual(result._fallback[1][0], 'y')
        self.assertEqual(result._fallback[1][1][-1]._value, 'e')

    def test_comparison_parse(self):
        result = QP.QueryCore_end.parseString("testing(RegMatch /test/)")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0]._data[CONSTRAINT_S][0], Comparison)
        self.assertEqual(result[0]._data[CONSTRAINT_S][0]._op, "RegMatch")

    def test_comparison_parse_2(self):
        result = QP.QueryCore.parseString("testing(RegMatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0]._data[CONSTRAINT_S][0], Comparison)
        self.assertEqual(result[0]._data[CONSTRAINT_S][0]._op, "RegMatch")

    def test_comparison_parse_variable(self):
        result = QP.QueryCore.parseString("$x(RegMatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0]._data[CONSTRAINT_S][0], Comparison)
        self.assertEqual(result[0]._data[CONSTRAINT_S][0]._op, "RegMatch")


    def test_comparison_in_clause(self):
        result = QP.clause.parseString("a.testing(RegMatch /test/).clause?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0][1]._data[CONSTRAINT_S][0], Comparison)
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][0]._op, "RegMatch")




if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Query_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
