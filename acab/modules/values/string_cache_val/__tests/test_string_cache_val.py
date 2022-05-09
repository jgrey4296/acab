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

from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.core.parsing.component_dsl import Component_DSL
from acab.modules.values.string_cache_val import value as StrCaV
from acab.modules.values.string_cache_val.caching_meta import StringCacheValueMeta
from acab.interfaces.value import ValueFactory
from acab.core.value.value import AcabValue
from acab.core.value.sentence import Sentence

class StringCacheValTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        ValueFactory.set(StrCaV.StringCacheValue, StrCaV.StringCacheSentence)

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL([], [], [])
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.build()
        # dsl()

    @classmethod
    def tearDownClass(cls):
        ValueFactory.set(AcabValue, Sentence)


    def test_simple(self):
        val = ValueFactory.value("test")
        self.assertIsInstance(val, StrCaV.StringCacheValue)
        self.assertTrue(hasattr(val, "_name"))
        self.assertIsInstance(val._name, int)
        self.assertEqual(val, val)

    def test_name_type(self):
        val = ValueFactory.value("test")
        self.assertTrue(hasattr(val, "_name"))
        self.assertIsInstance(val._name, int)
        self.assertEqual(val._name, hash("test"))
        self.assertEqual(val.name, "test")

    def test_cache_no_duplicate(self):
        val1 = ValueFactory.value("test")
        cache_len = StringCacheValueMeta.size()
        val2 = ValueFactory.value("test")
        self.assertEqual(cache_len, StringCacheValueMeta.size())

    def test_cache_increase(self):
        val1 = ValueFactory.value("test")
        cache_len = StringCacheValueMeta.size()
        val2 = ValueFactory.value("other")
        self.assertNotEqual(cache_len, StringCacheValueMeta.size())

    def test_value_not_cache(self):
        val1 = ValueFactory.value([1,2,3], name="blah")
        self.assertIsInstance(val1._name, int)
        self.assertEqual(val1.name, "blah")
        self.assertEqual(val1.value, [1,2,3])
        self.assertTrue(hasattr(val1, "_value"))
        self.assertEqual(val1._value, [1,2,3])

    def test_value_cache(self):
        val1 = ValueFactory.value("value", name="blah")
        self.assertIsInstance(val1._name, int)
        self.assertEqual(val1.name, "blah")
        self.assertEqual(val1.value, "value")
        self.assertTrue(hasattr(val1, "_value"))
        self.assertEqual(val1._value, hash("value"))
