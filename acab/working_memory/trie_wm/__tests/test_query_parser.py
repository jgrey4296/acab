import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

import acab.working_memory.trie_wm.parsing.QueryParser as QP
from acab.abstract.bootstrap_parser import BootstrapParser
from acab.abstract.type_base import REGEX
from acab.abstract.query import Query
from acab.abstract.sentence import Sentence
from acab.abstract.query import QueryComponent, QueryOp
from acab.abstract.production_operator import ProductionOperator
from acab.working_memory.trie_wm import util as KBU
from acab.modules.operators import query as QOP

NEGATION_S = AcabConfig.Get()("Parsing.Structure", "NEGATION_S")
FALLBACK_S = AcabConfig.Get()("Parsing.Structure", "FALLBACK_S")
CONSTRAINT_S = AcabConfig.Get()("Parsing.Structure", "CONSTRAINT_S")

class Trie_Query_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        bp = BootstrapParser()
        qmod = QOP.MODULE()
        qmod.assert_parsers(bp)
        QP.HOTLOAD_QUERY_OP << bp.query("operator.sugar")
        QP.HOTLOAD_QUERY_ANNOTATIONS << bp.query("query.annotation.*")


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets

    def test_basic_regex_comparison(self):
        result = QP.QUERY_OP_Internal.parseString(r'\operator.query.regmatch /blah/')[0]
        self.assertIsInstance(result, tuple)
        qc = result[1]
        self.assertIsInstance(qc, QueryComponent)
        self.assertEqual(qc.op.pprint(), 'operator.query.regmatch')
        self.assertEqual(qc._params[0].pprint(), '/blah/')
        self.assertEqual(qc._params[0].type, REGEX)

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
        self.assertTrue(result[-1].is_var)

    def test_basic_negated_clause(self):
        result = QP.clause.parseString('~a.b.c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(result._data[NEGATION_S])

    def test_basic_multi_clause(self):
        result = QP.clauses.parseString('a.b.c?, a.b.d?, a.b.e?')[0][1]
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result.clauses), 3)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertEqual(result.clauses[0][-1]._value, 'c')
        self.assertEqual(result.clauses[1][-1]._value, 'd')
        self.assertEqual(result.clauses[2][-1]._value, 'e')

    def test_basic_multi_clause_mixed_negation(self):
        result = QP.clauses.parseString('a.b.c?, ~a.b.d?, a.b.e?, ~a.b.f?')[0][1]
        self.assertIsInstance(result, Query)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertFalse(result.clauses[0]._data[NEGATION_S])
        self.assertTrue(result.clauses[1]._data[NEGATION_S])
        self.assertFalse(result.clauses[2]._data[NEGATION_S])
        self.assertTrue(result.clauses[3]._data[NEGATION_S])

    def test_basic_query_construction(self):
        result = QP.parseString('a.b.c?, a.b.d?, a.b.e?')
        self.assertIsInstance(result, Query)
        self.assertEqual(len(result.clauses), 3)

    def test_clause_fallback_strings(self):
        result = QP.clause.parseString('a.b.c? || $x:a.b!c, $y:b.d.e')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result._data[FALLBACK_S])
        self.assertEqual(len(result._data[FALLBACK_S]), 2)
        self.assertEqual(result._data[FALLBACK_S][0][0], 'x')
        self.assertEqual(result._data[FALLBACK_S][0][1][-1]._value, 'c')
        self.assertEqual(result._data[FALLBACK_S][1][0], 'y')
        self.assertEqual(result._data[FALLBACK_S][1][1][-1]._value, 'e')

    def test_comparison_parse(self):
        result = QP.QueryCore_end.parseString("testing(\operator.query.regmatch /test/)")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0]._data[CONSTRAINT_S][0], QueryComponent)
        self.assertEqual(result[0]._data[CONSTRAINT_S][0].op.pprint(), "operator.query.regmatch")

    def test_comparison_parse_2(self):
        result = QP.QueryCore.parseString("testing(\operator.query.regmatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0]._data[CONSTRAINT_S][0], QueryComponent)
        self.assertEqual(result[0]._data[CONSTRAINT_S][0].op.pprint(), "operator.query.regmatch")

    def test_comparison_parse_variable(self):
        result = QP.QueryCore.parseString("$x(\operator.query.regmatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0]._data[CONSTRAINT_S][0], QueryComponent)
        self.assertEqual(result[0]._data[CONSTRAINT_S][0].op.pprint(), "operator.query.regmatch")


    def test_comparison_in_clause(self):
        result = QP.clause.parseString("a.testing(\operator.query.regmatch /test/).clause?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0][1]._data[CONSTRAINT_S][0], QueryComponent)
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][0].op.pprint(), "operator.query.regmatch")


    def test_tag_query(self):
        result = QP.clause.parseString("a.testing(#testTag)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0][1]._data[CONSTRAINT_S][0], QueryComponent)
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][0].op.pprint(), "HasTag")

    def test_tag_list_query(self):
        result = QP.clause.parseString("a.testing(#first, #second, #third)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1]._data[CONSTRAINT_S]), 1)
        self.assertIsInstance(result[0][1]._data[CONSTRAINT_S][0], QueryComponent)
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][0].op.pprint(), "HasTag")

        tags = [x.name for x in result[0][1]._data[CONSTRAINT_S][0]._params]
        self.assertEqual(tags, ["first", "second", "third"])

    def test_tag_list_interaction(self):
        result = QP.clause.parseString("a.testing(#first, #second, \operator.query.regmatch /Test/)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1]._data[CONSTRAINT_S]), 2)
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][0].op.pprint(), "HasTag")
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][1].op.pprint(), "operator.query.regmatch")

    def test_tag_list_interaction_2(self):
        result = QP.clause.parseString("a.testing(\operator.query.regmatch /Test/, #test, #second)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1]._data[CONSTRAINT_S]), 2)
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][1].op.pprint(), "HasTag")
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][0].op.pprint(), "operator.query.regmatch")

    def test_tag_list_interaction_3(self):
        result = QP.clause.parseString("a.testing(#aTag, \operator.query.regmatch /Test/, #test, #second)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1]._data[CONSTRAINT_S]), 3)
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][0].op.pprint(), "HasTag")
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][1].op.pprint(), "operator.query.regmatch")
        self.assertEqual(result[0][1]._data[CONSTRAINT_S][2].op.pprint(), "HasTag")



if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Query_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
