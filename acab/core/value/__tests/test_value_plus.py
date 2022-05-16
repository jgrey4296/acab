#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

import acab.core.defaults.value_keys as DS
from acab.core.data.node import AcabNode
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue

AT_BIND_S = DS.AT_BIND
BIND_S    = DS.BIND

class BasicStatement(Instruction):

    def __contains__(self, val):
        return False

    def __len__(self):
        return 0

class AcabValueTests(unittest.TestCase):

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

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    #----------
    #use testcase snippets
    def test_attach_statement(self):
        value = BasicStatement(["test"])
        sen = Sentence() << ["a", "b", "c", "d", "e"]
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)
        self.assertIsInstance(copied, Sentence)
        self.assertIsInstance(copied[-1], BasicStatement)
        self.assertIsInstance(copied[-1], Instruction)
        self.assertEqual(copied[-1].value, ["test"])
        self.assertEqual(copied[-1].name, "e")

    def test_attach_statement_with_tags(self):
        value = BasicStatement(["test"], tags=["testval"])
        sen = Sentence() << ["a", "b", "c", "d", "e"]
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)
        self.assertTrue(AcabValue("testval") in copied[-1].tags)
        self.assertIsInstance(copied[-1], Instruction)

    def test_statement_to_simple_value(self):
        """ Check a statement can be downgraded to a value """
        value = BasicStatement("test")
        self.assertIsInstance(value, Instruction)
        basic = value.to_word()
        self.assertIsInstance(basic, AcabValue)

