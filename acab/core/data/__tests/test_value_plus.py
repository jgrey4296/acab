#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.core.data.value import AcabValue
from acab.core.data.sentence import Sentence
from acab.core.data.instruction import Instruction
from acab.core.data.node import AcabNode

AT_BIND_S = config.prepare("Value.Structure", "AT_BIND")()
BIND_S    = config.prepare("Value.Structure", "BIND")()

class BasicStatement(Instruction):

    def __contains__(self, val):
        return False

    def __len__(self):
        return 0

class AcabValueTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    #----------
    #use testcase snippets
    def test_attach_statement(self):
        value = BasicStatement.build(["test"])
        sen = Sentence.build(["a", "b", "c", "d", "e"])
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)
        self.assertIsInstance(copied, Sentence)
        self.assertIsInstance(copied[-1], BasicStatement)
        self.assertIsInstance(copied[-1], Instruction)
        self.assertEqual(copied[-1].value, ["test"])
        self.assertEqual(copied[-1].name, "e")

    def test_attach_statement_with_tags(self):
        value = BasicStatement.build(["test"], tags=["testval"])
        sen = Sentence.build(["a", "b", "c", "d", "e"])
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)

        self.assertTrue(AcabValue.build("testval") in copied[-1].tags)
        self.assertIsInstance(copied[-1], Instruction)

    def test_statement_to_simple_value(self):
        """ Check a statement can be downgraded to a value """
        value = BasicStatement.build("test")
        self.assertIsInstance(value, Instruction)
        basic = value.to_word()
        self.assertIsInstance(basic, AcabValue)

