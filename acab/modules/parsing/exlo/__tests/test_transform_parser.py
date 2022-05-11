import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import re
import warnings

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

from acab.core.parsing import parsers as PU
from acab.core.value import default_structure as DS
from acab.core.value.instruction import (ProductionComponent,
                                         ProductionContainer,
                                         ProductionOperator)
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP
from acab.modules.parsing.exlo.parsers import TransformParser as TP


class Trie_Transform_Parser_Tests(unittest.TestCase):

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

        TP.HOTLOAD_TRANS_OP << PU.OPERATOR_SUGAR

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)
        TP.HOTLOAD_TRANS_OP << pp.Empty()

    #----------
    def test_transform_core(self):
        """ Check a transform can be parsed """
        result = TP.transform_core.parse_string("λa.b.c $x $y -> $z")[0]
        self.assertIsInstance(result, ProductionComponent)
        self.assertEqual(result.rebind.name, "z")
        self.assertIn(DS.OPERATOR, result.op.type)


    def test_transforms(self):
        """ Check multiple transforms can be parsed """
        result = TP.transforms.parse_string("  λa.b.c $x -> $y\n  λa.b.d $x -> $z")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 2)
        self.assertIn(DS.OPERATOR, result.clauses[0].op.type)
        self.assertIn(DS.OPERATOR, result.clauses[1].op.type)

    def test_non_path_operator(self):
        result = TP.transform_sugar.parse_string("$x ∈ $y -> $z")[0]
        self.assertIsInstance(result, ProductionComponent)
        self.assertEqual(result.rebind.name, "z")
        self.assertIn(DS.OPERATOR, result.op.type)

    def test_transform_alias_statement(self):
        result = TP.transform_statement.parse_string("test(::χ):\n λa.b.c $x -> $y\nend")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(result.type, "_:TRANSFORM")

    def test_transform_statement(self):
        result = TP.transform_statement.parse_string("test(::TRANSFORM):\n λa.b.c $x -> $y\nend")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(result.type, "_:TRANSFORM")
