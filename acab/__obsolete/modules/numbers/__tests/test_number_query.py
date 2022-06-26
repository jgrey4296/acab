#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import warnings

import acab
from pyparsing import ParseException

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()

    from acab.core.printing import default_handlers as DH
    from acab.core.util.sentences import ProductionComponent
    from acab.core.value.instruction import ProductionOperator
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.modules.parsing.exlo import ActionParser as AP
    from acab.modules.parsing.exlo import FactParser as FP
    from acab.modules.parsing.exlo import QueryParser as QP
    from acab.modules.parsing.exlo import RuleParser as RP
    from acab.modules.parsing.exlo import TransformParser as TP
    from acab.modules.values import numbers
    from acab.modules.values.numbers.parsing import NumberParser as NP

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'OPERATOR',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

CONSTRAINT_S     = AcabConfig().value("Value.Structure", "CONSTRAINT")
OPERATOR_S       = AcabConfig().value("Value.Structure", "OPERATOR")
QUERY_FALLBACK_S = AcabConfig().value("Value.Structure", "QUERY_FALLBACK")

class NumberQueryTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

        # setup class
        NumberQueryTests.ns = numbers.MODULE()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

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
        result = QP.QUERY_OP_Internal.parse_string('λoperator.query.lt 20')[0]
        self.assertIsInstance(result, tuple)
        self.assertIsInstance(result[1], ProductionComponent)


    def test_basic_comparison(self):
        result = QP.constraints.parse_string('λoperator.query.lt 20, λoperator.query.gt 40, λoperator.query.neq $x, λoperator.query.eq $y, λoperator.query.regmatch /blah/')[0]
        self.assertEqual(result[0], CONSTRAINT_S)
        self.assertEqual(len(result[1]), 5)
        self.assertTrue(all([isinstance(x, ProductionComponent) for x in result[1]]))
        self.assertEqual(Printer.print(result[1][0].op), 'operator.query.lt')
        self.assertEqual(Printer.print(result[1][1].op), 'operator.query.gt')
        self.assertEqual(Printer.print(result[1][2].op), 'operator.query.neq')
        self.assertEqual(Printer.print(result[1][3].op), 'operator.query.eq')
        self.assertEqual(Printer.print(result[1][4].op), 'operator.query.regmatch')


    @unittest.skip
    def test_basic_query_core(self):
        result = QP.QueryCore.parse_string('a(λoperator.query.gt 20).')[0]
        self.assertTrue(CONSTRAINT_S in result._data)
        self.assertEqual(len(result._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result._data[CONSTRAINT_S][0], ProductionComponent)

    @unittest.skip
    def test_basic_query_core_multi_comparison(self):
        result = QP.QueryCore.parse_string('a(λoperator.query.gt 20, λoperator.query.lt 30).')[0]
        self.assertEqual(len(result._data[CONSTRAINT_S]), 2)
        self.assertTrue(all([isinstance(x, ProductionComponent) for x in result._data[CONSTRAINT_S]]))

    @unittest.skip
    def test_basic_query_core_with_exclusion(self):
        result = QP.QueryCore.parse_string('a(λoperator.query.gt 20)!')[0]
        self.assertEqual(result._data[OPERATOR_S], KBU.EXOP.EX)


    def test_clause_fallback(self):
        result = QP.clause.parse_string('a.b.c? || $x:2')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(QUERY_FALLBACK_S in result._data)
        self.assertIsNotNone(result._data[QUERY_FALLBACK_S])
        self.assertEqual(len(result._data[QUERY_FALLBACK_S]), 1)

        self.assertEqual(result._data[QUERY_FALLBACK_S][0][0], 'x')
        self.assertEqual(result._data[QUERY_FALLBACK_S][0][1][-1].value, 2)


    def test_clause_negated_fallback(self):
        with self.assertRaises(Exception):
            QP.clause.parse_string('~a.b.c? || $x:2')


    def test_clause_multi_fallback(self):
        result = QP.clause.parse_string('a.b.c? || $x:2, $y:5')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result._data[QUERY_FALLBACK_S])
        self.assertEqual(len(result._data[QUERY_FALLBACK_S]), 2)
        self.assertEqual(result._data[QUERY_FALLBACK_S][0][0], 'x')
        self.assertEqual(result._data[QUERY_FALLBACK_S][0][1][-1].value, 2)
        self.assertEqual(result._data[QUERY_FALLBACK_S][1][0], 'y')
        self.assertEqual(result._data[QUERY_FALLBACK_S][1][1][-1].value, 5)


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
            the_result = QP.parse_string(the_string)
            self.assertEqual(Printer.print(the_string, the_result).strip())


    def test_rule_binding_expansion(self):
        bindings = { "x" : FP.parse_string('a.b.c')[0],
                     "y" : FP.parse_string('d.e.f')[0],
                     "z" : FP.parse_string('x.y.z')[0] }
        result = RP.parse_string("a.rule: (::ρ)\n$y.b.$z?\n\nλoperator.transform.add $x 2 -> $y\n\n$y\nend")[0][-1]
        expanded = result.value.bind(bindings)
        # Expanding bindings makes a new rule, so its an AnonValue
        self.assertEqual(Printer.print(expanded).strip(),
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



