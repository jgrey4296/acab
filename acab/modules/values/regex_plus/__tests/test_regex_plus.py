#!/usr/bin/env python3
"""
Testing the regex plus value
"""
from __future__ import annotations
import logging as logmod
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext
import warnings

logging = logmod.getLogger(__name__)

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

##############################

# from acab.core.parsing import debug_funcs as DBF
# DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.core.parsing.component_dsl import Component_DSL
from acab.modules.values.regex_plus.module import RegexPlusFragment, RegMatch, RegexOp
from acab.modules.printing.basic_printer import BasicPrinter
from acab.interfaces import value as VI
from acab.modules.printing import default

class TestRegexPlus(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h    = logmod.FileHandler(LOG_FILE_NAME, mode="w")
        cls.file_h.setLevel(LOGLEVEL)

        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.register(RegexPlusFragment().build_dsl())
        cls.dsl.build()
        # dsl()

    @classmethod
    def tearDownClass(cls):
        logging.root.removeHandler(cls.file_h)

    def test_initial(self):
        result = self.dsl['word.value'].parse_string("/test/")[0]
        self.assertIsInstance(result, tuple)
        self.assertEqual(result[0], "_:REGEX.PLUS")
        self.assertIsInstance(result[1], RegexPlusFragment.RegexPlusInternal)

    def test_valbind(self):
        result = self.dsl['word.valbind'].parse_string("/test/")[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertEqual(result.type, "_:REGEX.PLUS")

    def test_print(self):
        printer = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC())
        printer.register(RegexPlusFragment().build_printers())
        val = self.dsl['word.valbind'].parse_string("/test/")[0]

        result = printer.pprint(val)
        self.assertEqual(result, "/test/")

    def test_sub_print(self):
        printer = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC())
        printer.register(RegexPlusFragment().build_printers())
        val = self.dsl['word.valbind'].parse_string("/test/blah/")[0]

        result = printer.pprint(val)
        self.assertEqual(result, "/test/blah/")

    def test_option_print(self):
        printer = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC())
        printer.register(RegexPlusFragment().build_printers())
        val = self.dsl['word.valbind'].parse_string("/test/blah/i")[0]

        result = printer.pprint(val)
        self.assertEqual(result, "/test/blah/i")

    def test_print(self):
        printer = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC())
        printer.register(RegexPlusFragment().build_printers())
        val = self.dsl['word.valbind'].parse_string(r"/(\w+?)(st)/\2_blah_\1/")[0]

        result = printer.pprint(val)
        self.assertEqual(result, r"/(\w+?)(st)/\2_blah_\1/")

    def test_match(self):
        val = self.dsl['word.valbind'].parse_string("/test/")[0]
        result = RegMatch()(VI.ValueFactory.value("test"), val)
        self.assertTrue(result)

    def test_match_subgroup(self):
        val = self.dsl['word.valbind'].parse_string("/te(?P<group>st)/")[0]
        result = RegMatch()(VI.ValueFactory.value("test"), val)
        self.assertTrue(result)
        self.assertIn("group", result)


    def test_match_fail(self):
        val = self.dsl['word.valbind'].parse_string("/test/")[0]
        result = RegMatch()(VI.ValueFactory.value("blah"), val)
        self.assertFalse(result)

    def test_sub(self):
        val = self.dsl['word.valbind'].parse_string("/test/blah/")[0]
        result = RegexOp()(VI.ValueFactory.value("test"), val)
        self.assertEqual(result, "blah")

    def test_sub_part(self):
        val = self.dsl['word.valbind'].parse_string("/test/blah/")[0]
        result = RegexOp()(VI.ValueFactory.value("a_test_atom"), val)
        self.assertEqual(result, "a_blah_atom")

    def test_sub_sen(self):
        val = self.dsl['word.valbind'].parse_string("/test/blah/")[0]
        result = RegexOp()(VI.ValueFactory.sen(["a", "test", "part_test"]), val)
        self.assertEqual(result, "_:a.blah.part_blah")

    def test_group_sub(self):
        val = self.dsl['word.valbind'].parse_string(r"/te(st)/\1_blah_\1/")[0]
        result = RegexOp()(VI.ValueFactory.value("a_test_atom"), val)
        self.assertEqual(result, "a_st_blah_st_atom")

    def test_sen_group_sub(self):
        val = self.dsl['word.valbind'].parse_string(r"/(\w+?)(st)/\2_blah_\1/")[0]
        result = RegexOp()(VI.ValueFactory.sen(["a", "test", "tast", "blah"]), val)
        self.assertEqual(result, "_:a.st_blah_te.st_blah_ta.blah")
