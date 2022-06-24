import logging as logmod
import unittest
from os.path import split, splitext

import pyparsing as pp

logging = logmod.getLogger(__name__)
##############################

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    import acab.core.defaults.value_keys as DS
    from acab.core.util.sentences import ProductionComponent

    from acab.core.parsing import parsers as PU
    from acab.core.value.instruction import (Instruction, ProductionContainer,
                                             ProductionOperator)
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.value import Value_i, Sentence_i
    from acab.modules.parsing.exlo.parsers import ActionParser as AP
    from acab.modules.parsing.exlo.parsers import FactParser as FP


class Trie_Action_Parser_Tests(unittest.TestCase):

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

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)
        AP.HOTLOAD_OPERATORS << pp.Empty()

    #----------
    #use testcase snippets
    def test_actions_parse(self):
        test_str = " λoperator.add\n λoperator.sub\n λoperator.mul"
        result = AP.actions.parse_string(test_str)[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result), 3)

    def test_action_definition(self):
        """ Check an action definition can be parsed without error """
        test_str = "test(::α):\n  λoperator.add a.b.c\nend"
        definition = AP.action_definition.parse_string(test_str)[0]
        self.assertEqual(definition.name, "test")

    def test_action_multi_def(self):
        test_str = "test(::α):\n λa.b.c $x\n λq.e.f $y $z\nend"
        definition = AP.action_definition.parse_string(test_str)[0]
        self.assertEqual(len(definition), 2)
        self.assertEqual(definition.name, "test")

    def test_parse_action_no_params(self):
        """ Check an action without parameters can be parsed """
        test_str = "λoperator.add"

        result = AP.action_component.parse_string(test_str)[0]
        self.assertIsInstance(result, Sentence)

    def test_multi_var_action(self):
        """ Check an action with multiple variables can be parsed """
        result = AP.action_component.parse_string("λa.b.c $x $y $z")[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[-1][-1], "unit")
        self.assertTrue(all([isinstance(x, Value_i) for x in result.params]))

    def test_actions(self):
        """ Test multiple actions can be parsed together """
        result = AP.actions.parse_string("  λa.b.c\n  a.b.d\n  λa.b.c blah bloo")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)

    # TODO test  sugar, test sentences, test values, test multi params
    def test_action_sugar_unary(self):
        result = AP.action_sugar_unary.parse_string("∈ $x")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIn(DS.OPERATOR, result[0].type)

    def test_action_sugar_bianry(self):
        result = AP.action_sugar_binary.parse_string("$y ∈ $x")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIn(DS.OPERATOR, result[0].type)

    def test_action_statement(self):
        result = AP.action_definition.parse_string("test(::ACTION):\n λa.b.c\nend")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(result.type, f"_:{DS.INSTR_PRIM}.{DS.CONTAINER_PRIM}.{DS.ACTION_COMPONENT}")

    def test_action_alias_statement(self):
        result = AP.action_definition.parse_string("test(::ACTION):\n λa.b.c\nend")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(result.type, f"_:{DS.INSTR_PRIM}.{DS.CONTAINER_PRIM}.{DS.ACTION_COMPONENT}")
