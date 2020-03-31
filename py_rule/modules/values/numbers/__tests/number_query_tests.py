#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.modules.values.numbers.parsing import NumberParser as NP
from py_rule.working_memory.trie_wm.parsing import ActionParser as AP
from py_rule.working_memory.trie_wm.parsing import RuleParser as RP
from py_rule.working_memory.trie_wm.parsing import TransformParser as TP
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.working_memory.trie_wm.parsing import QueryParser as QP
from py_rule.abstract import action
from py_rule.abstract.comparison import Comparison, CompOp
from py_rule.abstract.sentence import Sentence
from py_rule import util
from py_rule.modules.operators.operator_module import OperatorSpec
from py_rule.modules.values.numbers.number_module import NumberSpecification
from py_rule.working_memory.trie_wm.trie_working_memory import TrieWM
from py_rule.working_memory.trie_wm import util as KBU


class NumberQueryTests(unittest.TestCase):
    os = None
    ns = None

    @classmethod
    def setUpClass(cls):
        NumberQueryTests.os = OperatorSpec()
        NumberQueryTests.ns = NumberSpecification()

    def setUp(self):
        self.trie = TrieWM()
        self.trie.add_modules([NumberQueryTests.os, NumberQueryTests.ns])
        self.trie.build_operator_parser()

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass


    def test_basic_comp_internal(self):
        result = QP.COMP_Internal.parseString('LT 20')[0]
        self.assertIsInstance(result, Comparison)


    def test_basic_comparison(self):
        result = QP.constraints.parseString('LT 20, GT 40, NEQ $x, EQ $y, RegMatch /blah/')[0]
        self.assertEqual(result[0], util.CONSTRAINT_S)
        self.assertEqual(len(result[1]), 5)
        self.assertTrue(all([isinstance(x, Comparison) for x in result[1]]))
        self.assertEqual(result[1][0]._op, 'LT')
        self.assertEqual(result[1][1]._op, 'GT')
        self.assertEqual(result[1][2]._op, 'NEQ')
        self.assertEqual(result[1][3]._op, 'EQ')
        self.assertEqual(result[1][4]._op, 'RegMatch')


    def test_basic_query_core(self):
        result = QP.QueryCore.parseString('a(GT 20).')[0]
        self.assertTrue(util.CONSTRAINT_S in result._data)
        self.assertEqual(len(result._data[util.CONSTRAINT_S]), 1)
        self.assertIsInstance(result._data[util.CONSTRAINT_S][0], Comparison)


    def test_basic_query_core_multi_comparison(self):
        result = QP.QueryCore.parseString('a(GT 20, LT 30).')[0]
        self.assertEqual(len(result._data[util.CONSTRAINT_S]), 2)
        self.assertTrue(all([isinstance(x, Comparison) for x in result._data[util.CONSTRAINT_S]]))


    def test_basic_query_core_with_exclusion(self):
        result = QP.QueryCore.parseString('a(GT 20)!')[0]
        self.assertEqual(result._data[util.OPERATOR_S], KBU.EXOP.EX)


    def test_clause_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(util.FALLBACK_S in result._data)
        self.assertIsNotNone(result._data[util.FALLBACK_S])
        self.assertEqual(len(result._data[util.FALLBACK_S]), 1)

        self.assertEqual(result._data[util.FALLBACK_S][0][0], 'x')
        self.assertEqual(result._data[util.FALLBACK_S][0][1][-1]._value, 2)


    def test_clause_negated_fallback(self):
        with self.assertRaises(Exception):
            QP.clause.parseString('~a.b.c? || $x:2')


    def test_clause_multi_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2, $y:5')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result._data[util.FALLBACK_S])
        self.assertEqual(len(result._data[util.FALLBACK_S]), 2)
        self.assertEqual(result._data[util.FALLBACK_S][0][0], 'x')
        self.assertEqual(result._data[util.FALLBACK_S][0][1][-1]._value, 2)
        self.assertEqual(result._data[util.FALLBACK_S][1][0], 'y')
        self.assertEqual(result._data[util.FALLBACK_S][1][1][-1]._value, 5)


    def test_fact_str_equal(self):
        queries = ["a.b.c?", "a.b!c?", 'a.b."a string".c?',
                   'a.b!"a string"!c?', 'a.b(GT 20)?',
                   'a.$b?', 'a!$b?', 'a.$b(GT $c)?',
                   'a.$b(GT 20, LT 40, NEQ $x, EQ $y)?',
                   '~a.b.c?', '~a!b.c?',
                   'a.$b(RegMatch /blah/)?',
                   'a.b.c? || $x:2',
                   'a.b.d? || $x:5, $y:blah']
                   # 'a.b.c(^$x)?']
        parsed = [QP.parseString(x) for x in queries]
        zipped = zip(queries, parsed)
        for a,q in zipped:
            self.assertEqual(a,str(q))


    def test_rule_binding_expansion(self):
        bindings = { "x" : FP.parseString('a.b.c')[0],
                     "y" : FP.parseString('d.e.f')[0],
                     "z" : FP.parseString('x.y.z')[0] }
        result = RP.parseString("ρ::a.rule:\n$y.b.$z?\n\n$x + 2 -> $y\n\n+($y)\n\nend")[0][-1]
        expanded = result._value.expand_bindings(bindings)
        self.assertEqual(str(expanded),
                         "a.rule:\n\td.e.f.b.x.y.z?\n\n\t$x + 2 -> $y\n\n\t+(d.e.f)\nend")


    def test_query_alpha_comp(self):
        """ Check that alpha comparisons work """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(EQ 20)?')
        self.assertTrue(result)


    def test_query_alpha_comp_fails(self):
        """ Check that alpha comparisons can fail """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(EQ 30)?')
        self.assertFalse(result)


    def test_query_alpha_comp_GT(self):
        """ Check that other comparisons from equality can be tested for """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(GT 10)?')
        self.assertTrue(result)


    def test_query_fail(self):
        """ Check that other comparisons can fail """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(GT 30)?')
        self.assertFalse(result)


    def test_query_multi_bind_comp(self):
        """ Check that bindings hold across clauses """
        self.trie.add('a.b.20, a.c.30, a.d.40')
        result = self.trie.query('a.c.$x?, a.$y(NEQ c).$v(GT $x)?')
        self.assertTrue(result)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['x'], 30)
        self.assertEqual(result[0]['y'], 'd')
        self.assertEqual(result[0]['v'], 40)


    def test_query_multi_alts(self):
        """ Check that queries with bindings provide enumerated alternatives """
        self.trie.add('a.b.20, a.c.30, a.d.40, a.e.50')
        result = self.trie.query('a.c.$x?, a.$y(NEQ c).$v(GT $x)?')
        self.assertTrue(result)
        self.assertEqual(len(result), 2)



if __name__ == "__main__":
    #run python $filename to use this logging setup
    #using python -m unittest $filename won't
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
