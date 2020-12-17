import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get("acab/abstract/config")

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.containers.production_abstractions import ProductionOperator, ProductionComponent, ProductionContainer, ProductionStructure
from acab.modules.operators import query as QOP
from acab.working_memory.trie_wm import util as KBU
from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import FactParser as FP
from acab.working_memory.trie_wm.parsing import RuleParser as RP
import acab.working_memory.trie_wm.parsing.QueryParser as QP
from acab.abstract.printing.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH

QUERY_V = config.value("Structure.Components", "QUERY")
TRANSFORM_V = config.value("Structure.Components", "TRANSFORM")
ACTION_V = config.value("Structure.Components", "ACTION")

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR,
              ProductionStructure: ([], None)}
# TODO rule:

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'OPERATOR',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

class Trie_Rule_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        # setup class
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
    def test_init(self):
        self.assertIsNotNone(RP)

    def test_name_empty_rule_parse(self):
        result = RP.parseString("a.rule.x: (::ρ) end")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertEqual(result[0][-1].name, "x")

    def test_multi_empty_rules(self):
        result = RP.parseString("a.rule.x: (::ρ) end\n\na.second.rule: (::ρ) end")
        self.assertEqual(len(result),2)
        self.assertTrue(all([isinstance(x[-1], ProductionStructure) for x in result]))

    def test_rule_with_query(self):
        result = RP.parseString("a.rule.x: (::ρ)\na.b.c?\n\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)

    def test_rule_with_multi_clause_query(self):
        result = RP.parseString("a.rule.x: (::ρ)\na.b.c?\na.b.d?\n\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)
        self.assertEqual(len(result[0][-1][QUERY_V]), 2)

    def test_rule_with_multi_clauses_in_one_line(self):
        result = RP.parseString("a.rule.x: (::ρ)\na.b.c?, a.b.d?\n\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)
        self.assertEqual(len(result[0][-1][QUERY_V]), 2)

    def test_rule_with_binding_query(self):
        result = RP.parseString("a.rule.x: (::ρ)\na.b.$x?\n\nend")
        self.assertEqual(len(result),1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNotNone(result[0][-1][QUERY_V])
        self.assertIsInstance(result[0][-1][QUERY_V], ProductionContainer)
        self.assertEqual(len(result[0][-1][QUERY_V]), 1)

    def test_rule_with_actions(self):
        result = RP.parseString("a.rule.x: (::ρ)\nλoperator.action.add a.b.c\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 1)

    def test_multi_action_rule(self):
        result = RP.parseString("a.rule.x: (::ρ)\nλoperator.action.add a.b.c\nλoperator.action.add ~a.b.d\nend")
        self.assertEqual(len(result[0]), 3)
        self.assertIsInstance(result[0], Sentence)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 2)

    def test_multi_action_single_line_rule(self):
        result = RP.parseString("a.rule.x: (::ρ)\n\nλoperator.action.add a.b.c, λoperator.action.add ~a.b.d\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 2)

    @unittest.skip("awaiting printer")
    def test_name_empty_rule_print(self):
        result = RP.parseString("a.rule.x: (::ρ) end")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertEqual(Printer.print(result[0][-1]).strip(), "x: (::ρ) end")

    @unittest.skip("awaiting printer")
    def test_rule_simple_binding_expansion(self):
        bindings = { "x" : FP.parseString('a.b.c')[0] }
        result = RP.parseString("a.rule.x: (::ρ)\n\n$x?\n\nend")[0]
        expanded = result[-1].bind(bindings)
        self.assertEqual(Printer.print(expanded).strip(),
                         "AnonRule: (::ρ)\n    a.b.c?\nend")

    @unittest.skip("awaiting printer")
    def test_rule_tags(self):
        the_str = 'a.test.rule.x: (::ρ)\n    #blah, #blee, #bloo\n\n    a.b.c?\n\n    λoperator.action.add a.b.c\nend'
        result = RP.parseString(the_str)[0]
        self.assertIsInstance(result[-1], ProductionStructure)
        self.assertEqual(Printer.print(result).strip(), the_str)
        tags = [x for x in result[-1]._tags]
        self.assertTrue(all(x in tags for x in ["blah","bloo","blee"]))

