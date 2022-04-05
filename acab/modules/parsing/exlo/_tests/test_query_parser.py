import logging as logmod
import unittest
from os.path import split, splitext

import pyparsing as pp

logging = logmod.getLogger(__name__)

import acab

config = acab.setup()

import acab.modules.parsing.exlo.parsers.FactParser as FP
import acab.modules.parsing.exlo.parsers.QueryParser as QP
from acab.core.data.default_structure import BIND, NEGATION, QUERY_FALLBACK, QUERY
from acab.core.data.instruction import (Instruction, ProductionComponent,
                                        ProductionContainer,
                                        ProductionOperator)
from acab.core.data.sentence import Sentence
from acab.core.data.value import AcabValue
from acab.core.parsing import parsers as PU
from acab.modules.operators import query as QOP

CONSTRAINT_V     = config.prepare("Parse.Structure", "CONSTRAINT")()
REGEX_PRIM       = config.prepare("Type.Primitive", "REGEX")()

class Trie_Query_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.DEBUG)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)
        logging.setLevel(logmod.DEBUG)

    #----------
    #use testcase snippets
    def setUp(self):
        FP.HOTLOAD_SEN_POSTS << QP.query_sen_post_annotation

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
        result = QP.clauses.parse_string('  a.b.c?\n  a.b.d?\n  a.b.e?\n')[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertEqual(result.clauses[0][-1].value, 'c')
        self.assertEqual(result.clauses[1][-1].value, 'd')
        self.assertEqual(result.clauses[2][-1].value, 'e')

    def test_basic_multi_clause_mixed_negation(self):
        """ Check multiple queries of mixed positive and negative type can be parsed """
        result = QP.clauses.parse_string(' a.b.c?\n ~a.b.d?\n a.b.e?\n ~a.b.f?\n')[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertFalse(result.clauses[0].data[NEGATION])
        self.assertTrue(result.clauses[1].data[NEGATION])
        self.assertFalse(result.clauses[2].data[NEGATION])
        self.assertTrue(result.clauses[3].data[NEGATION])

    def test_basic_query_construction(self):
        result = QP.clauses.parse_string(' a.b.c?\n a.b.d?\n a.b.e?\n')[0]
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


    def test_query_statement(self):
        result = QP.query_statement.parse_string("query(::γ):\n  a.b.c?\n  d.e.f?\n  a.b.$x?\nend")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)
