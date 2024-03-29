import logging as logmod
import unittest
from os.path import split, splitext

import pyparsing as pp

logging = logmod.getLogger(__name__)

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    from acab.core.defaults.value_keys import ACTION_COMPONENT as ACTION_V
    from acab.core.defaults.value_keys import QUERY_COMPONENT as QUERY_V
    from acab.core.defaults.value_keys import TRANSFORM_COMPONENT as TRANSFORM_V


    from acab.core.parsing import parsers as PU
    from acab.core.util.sentences import ProductionComponent
    from acab.core.value.instruction import (ProductionContainer,
                                            ProductionOperator,
                                            ProductionStructure)
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.modules.parsing.exlo.parsers import ActionParser as AP
    from acab.modules.parsing.exlo.parsers import FactParser as FP
    from acab.modules.parsing.exlo.parsers import QueryParser as QP
    from acab.modules.parsing.exlo.parsers import RuleParser as RP
    from acab.modules.parsing.exlo import util as EXu

class Trie_Rule_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

        AP.HOTLOAD_OPERATORS << PU.OPERATOR_SUGAR
        QP.HOTLOAD_QUERY_OP  << PU.OPERATOR_SUGAR

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

        AP.HOTLOAD_OPERATORS << pp.Empty()
        QP.HOTLOAD_QUERY_OP  << pp.Empty()


    def setUp(self):
        FP.HOTLOAD_SEN_POSTS << QP.query_sen_post_annotation

    def tearDown(self):
        FP.HOTLOAD_SEN_POSTS <<= pp.NoMatch()

    #----------
    #use testcase snippets
    def test_init(self):
        """ Check the basic rule parser exists """
        self.assertIsNotNone(RP)

    def test_empty_rule_body(self):
        """ Check a rule can be empty """
        result = RP.rule_body.parse_string("")[0]
        self.assertIsInstance(result, ProductionStructure)

    def test_name_empty_rule_parse(self):
        result = RP.rule.parse_string("x(::ρ):\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertEqual(result.type, f"_:{EXu.INSTR}.{EXu.STRUCT}.{EXu.RULE_PRIM}")
        self.assertEqual(result.name, "x")

    def test_non_alias_empty_rule(self):
        result = RP.rule.parse_string("x(::RULE):\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertEqual(result.type, f"_:{EXu.INSTR}.{EXu.STRUCT}.{EXu.RULE_PRIM}")
        self.assertEqual(result.name, "x")

    @unittest.skip
    def test_multi_empty_rules(self):
        result = RP.parse_string("x(::ρ):\n\nend\n\na.second.rule:\n\nend")
        self.assertEqual(len(result),2)
        self.assertTrue(all([isinstance(x[-1], ProductionStructure) for x in result]))

    def test_rule_with_query(self):
        result = RP.rule.parse_string("x(::ρ):\n a.b.c?\nend")[0]
        self.assertEqual(result.name, "x")
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)


    def test_rule_with_multi_clause_query(self):
        result = RP.parse_string("x(::ρ):\n  a.b.c?\n  a.b.d?\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)
        self.assertEqual(len(result[QUERY_V]), 2)


    @unittest.skip("one liners need work")
    def test_rule_with_multi_clauses_in_one_line(self):
        result = RP.parse_string("x(::ρ):\n a.b.c?, a.b.d?\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)
        self.assertEqual(len(result[QUERY_V]), 2)


    def test_rule_with_binding_query(self):
        result = RP.parse_string("x(::ρ):\na.b.$x?\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertIsNotNone(result[QUERY_V])
        self.assertIsInstance(result[QUERY_V], ProductionContainer)
        self.assertEqual(len(result[QUERY_V]), 1)


    def test_rule_with_actions(self):
        result = RP.parse_string("x(::ρ):\nλoperator.action.add a.b.c\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertFalse(result[QUERY_V])
        self.assertFalse(result[TRANSFORM_V])
        self.assertEqual(len(result[ACTION_V]), 1)


    def test_multi_action_rule(self):
        result = RP.parse_string("x(::ρ):\n  λoperator.action.add a.b.c\n  λoperator.action.add ~a.b.d\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertTrue(all([x in result for x in [QUERY_V, TRANSFORM_V, ACTION_V]]))
        self.assertFalse(result.structure[QUERY_V])
        self.assertFalse(result.structure[TRANSFORM_V])
        self.assertEqual(len(result.structure[ACTION_V]), 2)

    @unittest.skip("one liners need work")
    def test_multi_action_single_line_rule(self):
        result = RP.parse_string("x(::ρ):\n λoperator.action.add a.b.c, λoperator.action.add ~a.b.d\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1], ProductionStructure)
        self.assertIsNone(result[0][-1][QUERY_V])
        self.assertIsNone(result[0][-1][TRANSFORM_V])
        self.assertEqual(len(result[0][-1][ACTION_V]), 2)

    def test_query_and_transform_rule(self):
        result = RP.rule.parse_string("x(::ρ):\n  a.b.c?\n  d.e.f?\n\n  λa.b.c $x -> $y\nend")[0]
        self.assertIsInstance(result, ProductionStructure)

    def test_query_and_transform_and_action_rule(self):
        result = RP.rule.parse_string("rule(::ρ):\n  a.b.c?\n  d.e.f?\n\n  λa.b.c $x -> $y\n\n a.b.c\nend")[0]
        self.assertIsInstance(result, ProductionStructure)

    def test_empty_rule_with_arg(self):
        result = RP.rule.parse_string("rule(::ρ):\n | $x |\n\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertEqual(len(result.params), 1)

    def test_empty_rule_with_args(self):
        result = RP.rule.parse_string("rule(::ρ):\n | $x, $y |\n\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertEqual(len(result.params), 2)

    def test_empty_rule_with_tags(self):
        result = RP.rule.parse_string("rule(::ρ):\n #test.blah.bloo\n\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertEqual(len(result.tags), 3)

    def test_empty_rule_with_multiline_tags(self):
        result = RP.rule.parse_string("rule(::ρ):\n #test.blah.bloo\n#aweg.foo.agjgj\n\nend")[0]
        self.assertIsInstance(result, ProductionStructure)
        self.assertEqual(len(result.tags), 6)
