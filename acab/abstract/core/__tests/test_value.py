#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.abstract.core.values import AcabValue, AcabStatement
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode

AT_BIND_S = config.prepare("Value.Structure", "AT_BIND")()
BIND_S    = config.prepare("Value.Structure", "BIND")()

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
    def test_construction(self):
        value = AcabValue("test")
        self.assertIsInstance(value, AcabValue)

    def test_attach_statement(self):
        value = AcabStatement(value="test")
        sen = Sentence.build(["a", "b", "c", "d", "e"])
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)
        self.assertIsInstance(copied[-1], AcabStatement)
        self.assertEqual(copied[-1].value, "test")
        self.assertEqual(copied[-1].name, "e")

    def test_attach_statement_with_tags(self):
        value = AcabStatement("test")
        value.tags.add('testval')
        sen = Sentence.build(["a", "b", "c", "d", "e"])
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)

        self.assertTrue("testval" in copied[-1].tags)
        self.assertIsInstance(copied[-1], AcabStatement)

    def test_has_tag(self):
        value = AcabValue("test")
        value.tags.update(["a"])
        self.assertTrue(value.has_tag("a"))

    def test_has_tag_fail(self):
        value = AcabValue("test")
        value.tags.update(["a"])
        self.assertFalse(value.has_tag("q"))

    def test_has_tag_multi(self):
        value = AcabValue("test")
        value.tags.update(["a", "b", "c"])
        self.assertTrue(value.has_tag("a", "b", "c"))

    def test_has_tag_multi_fail(self):
        value = AcabValue("test")
        value.tags.update(["a", "b", "c"])
        self.assertFalse(value.has_tag("a", "b", "c", "q"))

    def test_safe_make(self):
        value = AcabValue.safe_make("test")
        self.assertIsInstance(value, AcabValue)
        value2 = AcabValue.safe_make(value)
        self.assertIsInstance(value2, AcabValue)
        self.assertIsInstance(value2.value, str)


    def test_statement_to_simple_value(self):
        value = AcabStatement("test")
        self.assertIsInstance(value, AcabStatement)
        basic = value.to_word()
        self.assertIsInstance(basic, AcabValue)




    # set_data, apply_patams/tags,


    def test_value_copy(self):
        value = AcabValue("test")
        value.tags.update(["a"])
        copied = value.copy()
        copied.tags.update(["b"])
        self.assertEqual(value, copied)
        self.assertNotEqual(value.uuid, copied.uuid)
        self.assertTrue("a" in copied.tags)
        self.assertFalse("b" in value.tags)
