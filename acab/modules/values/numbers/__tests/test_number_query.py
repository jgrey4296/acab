#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from pyparsing import ParseException

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.core.sentence import Sentence
from acab.abstract.rule import action
from acab.abstract.rule.query import QueryComponent, QueryOp

from acab.modules.values import numbers
from acab.modules.values.numbers.parsing import NumberParser as NP

from acab.working_memory.trie_wm import util as KBU
from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import FactParser as FP
from acab.working_memory.trie_wm.parsing import QueryParser as QP
from acab.working_memory.trie_wm.parsing import RuleParser as RP
from acab.working_memory.trie_wm.parsing import TransformParser as TP
from acab.working_memory.trie_wm.trie_working_memory import TrieWM

CONSTRAINT_S = AcabConfig.Get()("Parsing.Structure", "CONSTRAINT_S")
OPERATOR_S = AcabConfig.Get()("Parsing.Structure", "OPERATOR_S")
FALLBACK_S = AcabConfig.Get()("Parsing.Structure", "FALLBACK_S")

class NumberQueryTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
        NumberQueryTests.ns = numbers.MODULE()

    def setUp(self):
        self.trie_wm = TrieWM()
        self.trie_wm.construct_parsers_from_fragments([NumberQueryTests.ns])

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass


    def test_basic_comp_internal(self):
        result = QP.QUERY_OP_Internal.parseString('λoperator.query.lt 20')[0]
        self.assertIsInstance(result, tuple)
        self.assertIsInstance(result[1], QueryComponent)


    def test_basic_comparison(self):
        result = QP.constraints.parseString('λoperator.query.lt 20, λoperator.query.gt 40, λoperator.query.neq $x, λoperator.query.eq $y, λoperator.query.regmatch /blah/')[0]
        self.assertEqual(result[0], CONSTRAINT_S)
        self.assertEqual(len(result[1]), 5)
        self.assertTrue(all([isinstance(x, QueryComponent) for x in result[1]]))
        self.assertEqual(result[1][0].op.pprint(), 'operator.query.lt')
        self.assertEqual(result[1][1].op.pprint(), 'operator.query.gt')
        self.assertEqual(result[1][2].op.pprint(), 'operator.query.neq')
        self.assertEqual(result[1][3].op.pprint(), 'operator.query.eq')
        self.assertEqual(result[1][4].op.pprint(), 'operator.query.regmatch')


    def test_basic_query_core(self):
        result = QP.QueryCore.parseString('a(λoperator.query.gt 20).')[0]
        self.assertTrue(CONSTRAINT_S in result._data)
        self.assertEqual(len(result._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result._data[CONSTRAINT_S][0], QueryComponent)


    def test_basic_query_core_multi_comparison(self):
        result = QP.QueryCore.parseString('a(λoperator.query.gt 20, λoperator.query.lt 30).')[0]
        self.assertEqual(len(result._data[CONSTRAINT_S]), 2)
        self.assertTrue(all([isinstance(x, QueryComponent) for x in result._data[CONSTRAINT_S]]))


    def test_basic_query_core_with_exclusion(self):
        result = QP.QueryCore.parseString('a(λoperator.query.gt 20)!')[0]
        self.assertEqual(result._data[OPERATOR_S], KBU.EXOP.EX)


    def test_clause_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(FALLBACK_S in result._data)
        self.assertIsNotNone(result._data[FALLBACK_S])
        self.assertEqual(len(result._data[FALLBACK_S]), 1)

        self.assertEqual(result._data[FALLBACK_S][0][0], 'x')
        self.assertEqual(result._data[FALLBACK_S][0][1][-1]._value, 2)


    def test_clause_negated_fallback(self):
        with self.assertRaises(Exception):
            QP.clause.parseString('~a.b.c? || $x:2')


    def test_clause_multi_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2, $y:5')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result._data[FALLBACK_S])
        self.assertEqual(len(result._data[FALLBACK_S]), 2)
        self.assertEqual(result._data[FALLBACK_S][0][0], 'x')
        self.assertEqual(result._data[FALLBACK_S][0][1][-1]._value, 2)
        self.assertEqual(result._data[FALLBACK_S][1][0], 'y')
        self.assertEqual(result._data[FALLBACK_S][1][1][-1]._value, 5)


    def test_fact_str_equal(self):
        queries = ["a.b.c?", "a.b!c?", 'a.b."a string".c?',
                   'a.b!"a string"!c?', 'a.b(λoperator.query.gt 20)?',
                   'a.$b?', 'a!$b?', 'a.$b(λoperator.query.gt $c)?',
                   'a.$b(λoperator.query.gt 20, λoperator.query.lt 40, λoperator.query.eq $x, λoperator.query.eq $y)?',
                   '~a.b.c?', '~a!b.c?',
                   'a.$b(λoperator.query.regmatch /blah/)?',
                   'a.b.c? || $x:2',
                   'a.b.d? || $x:5, $y:blah']
                   # 'a.b.c(^$x)?']

        for the_string in queries:
            the_result = QP.parseString(the_string)
            self.assertEqual(the_string, the_result.pprint().strip())


    def test_rule_binding_expansion(self):
        bindings = { "x" : FP.parseString('a.b.c')[0],
                     "y" : FP.parseString('d.e.f')[0],
                     "z" : FP.parseString('x.y.z')[0] }
        result = RP.parseString("a.rule: (::ρ)\n$y.b.$z?\n\nλoperator.transform.add $x 2 -> $y\n\n$y\nend")[0][-1]
        expanded = result.value.bind(bindings)
        # Expanding bindings makes a new rule, so its an AnonValue
        self.assertEqual(expanded.pprint().strip(),
                         "AnonRule: (::ρ)\n    d.e.f.b.x.y.z?\n\n    λoperator.transform.add $x 2 -> $y\n\n    λdefault_action d.e.f\nend")


    @unittest.skip("move this to engine logic tests")
    def test_query_alpha_comp(self):
        """ Check that alpha comparisons work """
        self.trie_wm.add('a.b.20')
        result = self.trie_wm.query('a.b.$x(λoperator.query.eq 20)?')
        self.assertTrue(result)

    @unittest.skip("move this to engine logic tests")
    def test_query_alpha_comp_fails(self):
        """ Check that alpha comparisons can fail """
        self.trie_wm.add('a.b.20')
        result = self.trie_wm.query('a.b.$x(λoperator.query.eq 30)?')
        self.assertFalse(result)

    @unittest.skip("move this to engine logic tests")
    def test_query_alpha_comp_gt(self):
        """ Check that other comparisons from equality can be tested for """
        self.trie_wm.add('a.b.20')
        result = self.trie_wm.query('a.b.$x(λoperator.query.gt 10)?')
        self.assertTrue(result)

    @unittest.skip("move this to engine logic tests")
    def test_query_fail(self):
        """ Check that other comparisons can fail """
        self.trie_wm.add('a.b.20')
        result = self.trie_wm.query('a.b.$x(λoperator.query.gt 30)?')
        self.assertFalse(result)

    @unittest.skip("move this to engine logic tests")
    def test_query_multi_bind_comp(self):
        """ Check that bindings hold across clauses """
        self.trie_wm.add('a.b.20, a.c.30, a.d.40')
        result = self.trie_wm.query('a.c.$x?, a.$y(λoperator.query.neq c).$v(λoperator.query.gt $x)?')
        self.assertTrue(result)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['x'].value, 30)
        self.assertEqual(result[0]['y'].value, 'd')
        self.assertEqual(result[0]['v'].value, 40)

    @unittest.skip("move this to engine logic tests")
    def test_query_multi_alts(self):
        """ Check that queries with bindings provide enumerated alternatives """
        self.trie_wm.add('a.b.20, a.c.30, a.d.40, a.e.50')
        result = self.trie_wm.query('a.c.$x?, a.$y(λoperator.query.neq c).$v(λoperator.query.gt $x)?')
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
