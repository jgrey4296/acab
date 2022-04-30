#!/usr/bin/env python3
"""
A Template for setting up a test case with an acab dsl ready
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
from acab.modules.values.sen_val.module import Sen_Val_Parser
from acab.core.util.log_formatter import AcabStringFormatter
from acab.core.parsing.component_dsl import Component_DSL

class SenValTests(unittest.TestCase):

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
        cls.dsl.register(Sen_Val_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.build()
        # dsl()

    def test_basic(self):
        result = self.dsl("a.test.[[sub.sentence]]")[0]
        self.assertIsInstance(result, VI.Sentence_i)
        self.assertIsInstance(result[-1], VI.Sentence_i)
        self.assertEqual(result, "_:a.test.sub.sentence")
        self.assertEqual(result[-1], "_:sub.sentence")

    def test_basic_flatten(self):
        result = self.dsl("a.test.[[sub.sentence]]")[0]
        flat = result.flatten()
        self.assertIsInstance(flat, VI.Sentence_i)
        self.assertNotIsInstance(flat[-1], VI.Sentence_i)
        self.assertEqual(flat, "_:a.test.sub.sentence")
        self.assertEqual(flat[-2:], "_:sub.sentence")

    def test_basic_sharp(self):
        result = self.dsl("a.test.[[sub.sentence]](♯)")[0]
        flat = result.flatten()
        self.assertIsInstance(flat, VI.Sentence_i)
        self.assertIsInstance(flat[-1], VI.Sentence_i)
        self.assertEqual(flat, "_:a.test.sub.sentence")
        self.assertEqual(flat[-1], "_:sub.sentence")

    def test_basic_sharp_head(self):
        result = self.dsl("♯a.test.[[sub.sentence]]")[0]
        self.assertIn(DS.FLATTEN, result.data)
        self.assertFalse(result.data[DS.FLATTEN])
        flat = result.flatten()
        self.assertIsInstance(flat, VI.Sentence_i)
        self.assertIsInstance(flat[-1], VI.Sentence_i)
        self.assertEqual(flat, "_:a.test.sub.sentence")
        self.assertEqual(flat[-1], "_:sub.sentence")
