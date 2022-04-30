#!/usr/bin/env python3
"""

"""
import logging as logmod
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import acab
import pyparsing as pp

if '@pytest_ar' in globals():
    from acab.core.parsing import debug_funcs as DBF
    DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)
##############################

config = acab.setup()

# from acab.core.parsing import debug_funcs as DBF
# DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

import acab.interfaces.value as VI
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.value import default_structure as DS
from acab.modules.context.context_instance import ContextInstance
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.values.binding import binding as B


class BindingLogicTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL([], [], [])
        cls.dsl.register(EXLO_Parser)
        cls.dsl.build()
        # dsl()
        #
    def test_no_change(self):
        source = self.dsl("test")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance()

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertIs(source, result)


    def test_no_change_var(self):
        source = self.dsl("$x")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance()

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertIs(source, result)

    def test_no_change_var_mismatch(self):
        source = self.dsl("$x")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        target = self.dsl("target")[0][0]
        self.assertIsInstance(target, VI.Value_i)
        ctx = ContextInstance({"y": target})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertEqual(source, result)

    def test_basic(self):
        source = self.dsl("$x")[0][0]
        self.assertIsInstance(source, VI.Value_i)
        target = self.dsl("target")[0][0]
        self.assertIsInstance(target, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(target, result)

    def test_basic_sentence_no_change(self):
        source = self.dsl("a.test.sentence")[0]
        self.assertIsInstance(source, VI.Sentence_i)
        ctx = ContextInstance()

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Sentence_i)
        self.assertEqual(source, result)

    def test_basic_sentence_simple_var_sub(self):
        source = self.dsl("a.test.$x")[0]
        target = self.dsl("sentence")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence")

    def test_basic_sentence_repeat_sub(self):
        source = self.dsl("a.test.$x.$x")[0]
        target = self.dsl("sentence")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.sentence")

    def test_basic_sentence_multi_sub(self):
        source = self.dsl("a.test.$x.$y")[0]
        target = self.dsl("sentence")[0]
        target2 = self.dsl("blah")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target, "y": target2})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.blah")

    def test_basic_sentence_sub_sentence_default_flatten(self):
        source = self.dsl("a.test.$x")[0]
        target = self.dsl("sentence.blah")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.blah")
        self.assertIsInstance(result[-1], VI.Value_i)
        self.assertEqual(result[-1], "blah")

    def test_basic_sentence_sub_sentence_explicit_flatten(self):
        source = self.dsl("a.test.$x(♭)")[0]
        target = self.dsl("sentence.blah")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.blah")
        self.assertEqual(len(result), 4)

    def test_basic_sentence_sub_sentence_explicit_sharp(self):
        source = self.dsl("a.test.$x(♯)")[0]
        target = self.dsl("sentence.blah")[0]
        self.assertIsInstance(source, VI.Value_i)
        ctx = ContextInstance({"x": target})

        result = B.bind(source, ctx)
        self.assertIsInstance(result, VI.Value_i)
        self.assertNotEqual(source, result)
        self.assertEqual(result, "_:a.test.sentence.blah")
        self.assertEqual(len(result), 3)
        self.assertIsInstance(result[-1], VI.Sentence_i)

