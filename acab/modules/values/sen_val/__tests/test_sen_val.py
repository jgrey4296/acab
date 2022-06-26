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

import warnings

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)



    import acab.core.defaults.value_keys as DS
    import acab.interfaces.value as VI
    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.core.util.log_formatter import AcabStringFormatter
    from acab.modules.context.context_instance import ContextInstance
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.modules.values.binding import binding as B
    from acab.modules.values.sen_val.module import Sen_Val_Parser


class SenValTests(unittest.TestCase):

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

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Sen_Val_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.build()
        # dsl()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_basic(self):
        result = self.dsl("a.test.[[sub.sentence]]")[0]
        self.assertIsInstance(result, VI.Sentence_i)
        self.assertIsInstance(result[-1], VI.Sentence_i)
        self.assertEqual(result, "_:a.test.[sub.sentence]")
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
        self.assertEqual(flat, "_:a.test.[sub.sentence]")
        self.assertEqual(flat[-1], "_:sub.sentence")

    def test_basic_sharp_head(self):
        result = self.dsl("♯a.test.[[sub.sentence]]")[0]
        self.assertIn(DS.FLATTEN, result.data)
        self.assertFalse(result.data[DS.FLATTEN])
        flat = result.flatten()
        self.assertIsInstance(flat, VI.Sentence_i)
        self.assertIsInstance(flat[-1], VI.Sentence_i)
        self.assertEqual(flat, "_:a.test.[sub.sentence]")
        self.assertEqual(flat[-1], "_:sub.sentence")


    def test_basic_sharp_head_internal(self):
        result = self.dsl("a.test.♯[[sub.sentence]]")[0]
        self.assertIn(DS.FLATTEN, result[-1].data)
        self.assertFalse(result[-1].data[DS.FLATTEN])
        flat = result.flatten()
        self.assertIsInstance(flat, VI.Sentence_i)
        self.assertIsInstance(flat[-1], VI.Sentence_i)
        self.assertEqual(flat, "_:a.test.[sub.sentence]")
        self.assertEqual(flat[-1], "_:sub.sentence")


    def test_valbind_flatten(self):
        result = self.dsl('$x(♭)')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertTrue(result[0].data[DS.FLATTEN])

    def test_sentence_flatten_head(self):
        result = self.dsl('♭a.b.$x')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertTrue(result.data[DS.FLATTEN])


    def test_sentence_flatten_head(self):
        result = self.dsl('♯a.b.$x')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertFalse(result.data[DS.FLATTEN])

    def test_valbind_flatten_head(self):
        result = self.dsl('a.b.♭$x')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertTrue(result[-1].data[DS.FLATTEN])

    def test_valbind_no_flatten(self):
        result = self.dsl('$x~♭')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertFalse(result[0].data[DS.FLATTEN])

    def test_valbind_no_flatten_as_sharp(self):
        result = self.dsl('$x(♯)')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertFalse(result[0].data[DS.FLATTEN])

    def test_valbind_no_flatten_as_sharp(self):
        result = self.dsl('$x♯')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertFalse(result[0].data[DS.FLATTEN])

    def test_valbind_flatten_as_not_sharp(self):
        result = self.dsl('$x~♯')[0]
        self.assertIsInstance(result, VI.Value_i)
        self.assertTrue(result[0].data[DS.FLATTEN])
