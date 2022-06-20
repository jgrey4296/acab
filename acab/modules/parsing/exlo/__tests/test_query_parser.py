import logging as logmod
import unittest
from os.path import split, splitext

import pyparsing as pp

logging = logmod.getLogger(__name__)
logging.setLevel(logmod.DEBUG)

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    import acab.modules.parsing.exlo.parsers.FactParser as FP
    import acab.modules.parsing.exlo.parsers.QueryParser as QP
    from acab.core.defaults import value_keys as DS
    from acab.core.defaults.value_keys import (BIND, NEGATION, OPERATOR, QUERY,
                                               QUERY_FALLBACK)
    from acab.core.parsing import parsers as PU
    from acab.core.parsing.annotation import ValueRepeatAnnotation
    from acab.core.util.sentences import ProductionComponent

    from acab.core.value.instruction import (Instruction, ProductionContainer,
                                             ProductionOperator)
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.modules.operators import query as QOP

CONSTRAINT_V     = config.prepare("Parse.Structure", "CONSTRAINT")()
REGEX_PRIM       = config.prepare("Type.Primitive", "REGEX")()


class Trie_Query_Parser_Tests(unittest.TestCase):

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

        QP.HOTLOAD_QUERY_OP << PU.OPERATOR_SUGAR

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)
        QP.HOTLOAD_QUERY_OP << pp.Empty()
    #----------
    #use testcase snippets
    def setUp(self):
        FP.HOTLOAD_SEN_POSTS << QP.query_sen_post_annotation
        FP.HOTLOAD_ANNOTATIONS << QP.word_query_constraint

    def tearDown(self):
        FP.HOTLOAD_SEN_POSTS <<= pp.NoMatch()

    def test_query_tail(self):
        result = FP.SENTENCE.parse_string("a.test.query?")[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result, "_:a.test.query")
        self.assertIn(BIND, result.data)
        self.assertIn(QUERY, result.data)


    def test_basic_clause(self):
        result = QP.SENTENCE.parse_string('a.b.c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[-1].value, 'c')
        self.assertIn(QUERY, result.data)

    def test_basic_clause_with_bind(self):
        result = QP.SENTENCE.parse_string('a.b.$c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[-1].value, 'c')
        self.assertTrue(result[-1].is_var)
        self.assertIn(QUERY, result.data)

    def test_basic_negated_clause(self):
        result = QP.SENTENCE.parse_string('~a.b.c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(result.data[NEGATION])
        self.assertIn(QUERY, result.data)

    def test_basic_multi_clause(self):
        """ Check multiple query clauses can be parsed """
        result = QP.clauses.parse_string('  a.b.c?\n  a.b.d?\n  a.b.e?')[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertEqual(result.clauses[0][-1].value, 'c')
        self.assertEqual(result.clauses[1][-1].value, 'd')
        self.assertEqual(result.clauses[2][-1].value, 'e')

    def test_basic_multi_clause_mixed_negation(self):
        """ Check multiple queries of mixed positive and negative type can be parsed """
        result = QP.clauses.parse_string(' a.b.c?\n ~a.b.d?\n a.b.e?\n ~a.b.f?')[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertFalse(result.clauses[0].data[NEGATION])
        self.assertTrue(result.clauses[1].data[NEGATION])
        self.assertFalse(result.clauses[2].data[NEGATION])
        self.assertTrue(result.clauses[3].data[NEGATION])

    def test_basic_query_construction(self):
        result = QP.clauses.parse_string(' a.b.c?\n a.b.d?\n a.b.e?')[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)

    @unittest.skip
    def test_clause_fallback_strings(self):
        result = QP.clauses.parse_string('a.b.c? || $x:a.b!c, $y:b.d.e\n')[0]
        self.assertIsInstance(result, ProductionContainer)
        r_clause = result.clauses[0]
        breakpoint()
        self.assertEqual(len(r_clause[-1].data[QUERY_FALLBACK]), 2)
        self.assertEqual(r_clause[-1].data[QUERY_FALLBACK][0][0], 'x')
        self.assertEqual(r_clause[-1].data[QUERY_FALLBACK][0][1][-1].value, 'c')
        self.assertEqual(r_clause[-1].data[QUERY_FALLBACK][1][0], 'y')
        self.assertEqual(r_clause[-1].data[QUERY_FALLBACK][1][1][-1].value, 'e')


    def test_query_alias_statement(self):
        result = QP.query_statement.parse_string("query(::γ):\n  a.b.c?\n  d.e.f?\n  a.b.$x?\nend")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)

    def test_query_statement(self):
        result = QP.query_statement.parse_string("query(::QUERY):\n  a.b.c?\n  d.e.f?\n  a.b.$x?\nend")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)

    def test_queries_with_params(self):
        result = QP.clauses.parse_string(" a.b.c?\n d.e(λa.b.q $y).f?\n g.h.i?")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)

    def test_basic_constraint_no_params(self):
        result = QP.basic_constraint.parse_string("λa.b.c")[0]
        self.assertIsInstance(result, ValueRepeatAnnotation)
        self.assertIsInstance(result.value, ProductionComponent)
        self.assertFalse(result.value.params)
        self.assertIn(OPERATOR, result.value.op.type)

    def test_basic_constraint_one_param(self):
        result = QP.basic_constraint.parse_string("λa.b.c $x")[0]
        self.assertIsInstance(result, ValueRepeatAnnotation)
        self.assertIsInstance(result.value, ProductionComponent)
        self.assertTrue(result.value.params)
        self.assertIn(OPERATOR, result.value.op.type)

    def test_basic_constraint_one_sen(self):
        result = QP.basic_constraint.parse_string("λa.b.c q.w.e")[0]
        self.assertIsInstance(result, ValueRepeatAnnotation)
        self.assertIsInstance(result.value, ProductionComponent)
        self.assertTrue(result.value.params)
        self.assertIn(OPERATOR, result.value.op.type)


    def test_basic_constraint_multi_params(self):
        result = QP.basic_constraint.parse_string("λa.b.c q.w.e $x $y")[0]
        self.assertIsInstance(result, ValueRepeatAnnotation)
        self.assertIsInstance(result.value, ProductionComponent)
        self.assertTrue(result.value.params)

    def test_basic_constraint_on_word(self):
        result = FP.SEN_WORD.parse_string("test(λa.b.c $x $y).")[0]
        self.assertIsInstance(result, AcabValue)

    def test_basic_constraint_non_op_path(self):
        result = QP.basic_constraint.parse_string("!!")[0]
        self.assertIsInstance(result, ValueRepeatAnnotation)
        self.assertIsInstance(result.value, ProductionComponent)
        self.assertFalse(result.value.params)
        self.assertIn(OPERATOR, result.value.op.type)

    def test_basic_constraint_args_are_sentences(self):
        result = QP.basic_constraint.parse_string("λa.b.c q.w.e")[0]
        self.assertIsInstance(result.value.params[0], Sentence)

    def test_basic_constraint_args_are_sentences_single_var(self):
        result = QP.basic_constraint.parse_string("λa.b.c $q")[0]
        self.assertIsInstance(result.value.params[0], Sentence)

    def test_basic_constraint_args_are_sentences_single_word(self):
        result = QP.basic_constraint.parse_string("λa.b.c q")[0]
        self.assertIsInstance(result.value.params[0], Sentence)

    def test_constraint_arg_is_sentence(self):
        result = FP.SEN_WORD.parse_string("test(λa.b.c $x $y).")[0]
        for arg in result.data[DS.CONSTRAINT][0].params:
            self.assertIsInstance(arg, Sentence)


    def test_constraint_arg_is_sentence2(self):
        result = FP.SEN_WORD.parse_string("test(== x.b.c y).")[0]
        for arg in result.data[DS.CONSTRAINT][0].params:
            self.assertIsInstance(arg, Sentence)
