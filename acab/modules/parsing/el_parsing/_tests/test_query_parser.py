import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get("acab/abstract/config")

from acab.abstract.parsing.parsers import HOTLOAD_VALUES, VALBIND

import acab.working_memory.trie_wm.parsing.FactParser as FP
import acab.working_memory.trie_wm.parsing.QueryParser as QP

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.containers.production_abstractions import ProductionOperator, ProductionContainer, ProductionComponent

from acab.modules.operators import query as QOP
from acab.working_memory.trie_wm import util as KBU

from acab.abstract.printing.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'exop',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

NEGATION_V       = AcabConfig.Get().value("Parse.Structure", "NEGATION")
QUERY_FALLBACK_V = AcabConfig.Get().value("Parse.Structure", "QUERY_FALLBACK")
CONSTRAINT_V     = AcabConfig.Get().value("Parse.Structure", "CONSTRAINT")
REGEX_PRIM       = AcabConfig.Get().value("Type.Primitive", "REGEX")

class Trie_Query_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        bp = BootstrapParser()
        qmod = QOP.MODULE()
        qmod.assert_parsers(bp)
        FP.HOTLOAD_QUERY_OP << bp.query("operator.sugar")
        FP.HOTLOAD_ANNOTATIONS << bp.query("query.annotation.*")


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets

    def test_basic_regex_comparison(self):
        result = FP.QUERY_OP_Internal.parseString(r'λoperator.query.regmatch /blah/')[0]
        self.assertIsInstance(result, tuple)
        qc = result[1]
        self.assertIsInstance(qc, ProductionComponent)
        self.assertEqual(Printer.print(qc.op), 'operator.query.regmatch')
        # Note the end modal '.'
        self.assertEqual(Printer.print(qc.params[0]), '/blah/.')
        # TODO convert this to a type system lookup
        self.assertEqual(qc.params[0].type, Sentence.build([REGEX_PRIM]))

    def test_basic_clause(self):
        result = QP.clause.parseString('a.b.c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[-1].value, 'c')
        self.assertEqual(result[-1].data['exop'], config.modal_enums['exop'].DOT)

    def test_basic_clause_with_bind(self):
        result = QP.clause.parseString('a.b.$c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[-1].value, 'c')
        self.assertEqual(result[-1].data['exop'], config.modal_enums['exop'].DOT)
        self.assertTrue(result[-1].is_var)

    def test_basic_negated_clause(self):
        result = QP.clause.parseString('~a.b.c?')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(result.data[NEGATION_V])

    def test_basic_multi_clause(self):
        result = QP.clauses.parseString('a.b.c?, a.b.d?, a.b.e?')[0][1]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertEqual(result.clauses[0][-1].value, 'c')
        self.assertEqual(result.clauses[1][-1].value, 'd')
        self.assertEqual(result.clauses[2][-1].value, 'e')

    def test_basic_multi_clause_mixed_negation(self):
        result = QP.clauses.parseString('a.b.c?, ~a.b.d?, a.b.e?, ~a.b.f?')[0][1]
        self.assertIsInstance(result, ProductionContainer)
        self.assertTrue(all([isinstance(x, Sentence) for x in result.clauses]))
        self.assertFalse(result.clauses[0].data[NEGATION_V])
        self.assertTrue(result.clauses[1].data[NEGATION_V])
        self.assertFalse(result.clauses[2].data[NEGATION_V])
        self.assertTrue(result.clauses[3].data[NEGATION_V])

    def test_basic_query_construction(self):
        result = QP.parseString('a.b.c?, a.b.d?, a.b.e?')
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)

    def test_clause_fallback_strings(self):
        result = QP.clause.parseString('a.b.c? || $x:a.b!c, $y:b.d.e')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result.data[QUERY_FALLBACK_V])
        self.assertEqual(len(result.data[QUERY_FALLBACK_V]), 2)
        self.assertEqual(result.data[QUERY_FALLBACK_V][0][0], 'x')
        self.assertEqual(result.data[QUERY_FALLBACK_V][0][1][-1].value, 'c')
        self.assertEqual(result.data[QUERY_FALLBACK_V][1][0], 'y')
        self.assertEqual(result.data[QUERY_FALLBACK_V][1][1][-1].value, 'e')

    def test_comparison_parse(self):
        result = FP.PARAM_BINDING_END.parseString("testing(λoperator.query.regmatch /test/)")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0].data[CONSTRAINT_V][0].op), "operator.query.regmatch")

    def test_comparison_parse_2(self):
        result = FP.PARAM_BINDING_CORE.parseString("testing(λoperator.query.regmatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0].data[CONSTRAINT_V][0].op), "operator.query.regmatch")

    def test_comparison_parse_variable(self):
        result = FP.PARAM_BINDING_CORE.parseString("$x(λoperator.query.regmatch /test/).")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0].data[CONSTRAINT_V][0].op), "operator.query.regmatch")


    def test_comparison_in_clause(self):
        result = QP.clause.parseString("a.testing(λoperator.query.regmatch /test/).clause?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0][1].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "operator.query.regmatch")


    def test_tag_query(self):
        result = QP.clause.parseString("a.testing(#testTag)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0][1].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")

    def test_tag_list_query(self):
        result = QP.clause.parseString("a.testing(#first, #second, #third)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 1)
        self.assertIsInstance(result[0][1].data[CONSTRAINT_V][0], ProductionComponent)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")

        tags = [x.name for x in result[0][1].data[CONSTRAINT_V][0]._params]
        self.assertEqual(tags, ["first", "second", "third"])

    def test_tag_list_interaction(self):
        result = QP.clause.parseString("a.testing(#first, #second, λoperator.query.regmatch /Test/)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 2)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][1].op), "operator.query.regmatch")

    def test_tag_list_interaction_2(self):
        result = QP.clause.parseString("a.testing(λoperator.query.regmatch /Test/, #test, #second)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 2)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][1].op), "HasTag")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "operator.query.regmatch")

    def test_tag_list_interaction_3(self):
        result = QP.clause.parseString("a.testing(#aTag, λoperator.query.regmatch /Test/, #test, #second)?")
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0][1].data[CONSTRAINT_V]), 3)
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][0].op), "HasTag")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][1].op), "operator.query.regmatch")
        self.assertEqual(Printer.print(result[0][1].data[CONSTRAINT_V][2].op), "HasTag")
