#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import warnings

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()

from acab.core.data.node import AcabNode
from acab.core.parsing import parsers as PU
from acab.core.parsing.param_core import ParamCore
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue


class ParamCoreTests(unittest.TestCase):

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

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_basic_core(self):
        """ Check a constructed statement can have tags """
        basic = ParamCore()
        result = basic.parse_string("test.")
        self.assertTrue(result)
        self.assertEqual(result[0], "test")


    def test_basic_core_no_modal(self):
        """ Check a constructed statement can have tags """
        basic = ParamCore(end=False)
        result = basic.parse_string("test")
        self.assertTrue(result)
        self.assertEqual(result[0], "test")

    def test_custom_end(self):
        basic = ParamCore(end=":")
        result = basic.parse_string("test:")
        self.assertTrue(result)
        self.assertEqual(result[0], "test")

    def test_annotation(self):
        basic = ParamCore(mid="blah")
        result = basic.parse_string("test(blah).")
        self.assertTrue(result)
        self.assertEqual(result[0], "test")
