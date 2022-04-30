#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as logmod
logging = logmod.getLogger(__name__)

import pyparsing as pp

import acab
acab.setup()

from acab.core.parsing.param_core import ParamCore
from acab.core.parsing import parsers as PU
from acab.core.value.value import AcabValue
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.data.node import AcabNode


class ParamCoreTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

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
