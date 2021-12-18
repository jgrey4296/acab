#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.core.data.value import AcabValue, Instruction
from acab.core.data.value import Sentence
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
    def test_safe_make(self):
        """ Check a value can be created safely with "safe_make" """
        value = AcabValue.safe_make("test",
                                    name="test value",
                                    data={"test data" : True})
        self.assertIsInstance(value, AcabValue)
        self.assertEqual(value.value, "test")
        self.assertEqual(value.name, "test_value")
        self.assertTrue("test data" in value.data)

    def test_construction(self):
        """ Check a value can be created using a normal constructor """
        value = AcabValue("test")
        self.assertIsInstance(value, AcabValue)

    def test_attach_statement(self):
        # TODO this is duplicated from test sentence
        value = BasicStatement(value="test")
        sen = Sentence.build(["a", "b", "c", "d", "e"])
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)
        self.assertIsInstance(copied[-1], Instruction)
        self.assertEqual(copied[-1].value, "test")
        self.assertEqual(copied[-1].name, "e")

    def test_attach_statement_with_tags(self):
        # TODO move to test sentence
        value = BasicStatement("test")
        value.tags.add('testval')
        sen = Sentence.build(["a", "b", "c", "d", "e"])
        self.assertEqual(sen[-1].value, "e")
        self.assertIsInstance(sen[-1].value, str)
        copied = sen.attach_statement(value)

        self.assertTrue("testval" in copied[-1].tags)
        self.assertIsInstance(copied[-1], Instruction)

    def test_has_tag(self):
        """ Check a sentence can report whether it has a tag """
        value = AcabValue("test")
        value.tags.update(["a"])
        self.assertTrue(value.has_tag("a"))

    def test_has_tag_fail(self):
        """ check a value can report it doesn't have a tag """
        value = AcabValue("test")
        value.tags.update(["a"])
        self.assertFalse(value.has_tag("q"))

    def test_has_tag_multi(self):
        """ Check a value can report it has multiple tags """
        value = AcabValue("test")
        value.tags.update(["a", "b", "c"])
        self.assertTrue(value.has_tag("a", "b", "c"))

    def test_has_tag_multi_fail(self):
        """ Check a value can report it doesn't have all specified tags """
        value = AcabValue("test")
        value.tags.update(["a", "b", "c"])
        self.assertFalse(value.has_tag("a", "b", "c", "q"))

    def test_safe_make(self):
        """ Check a value doesn't build to contain a value """
        value = AcabValue.safe_make("test")
        self.assertIsInstance(value, AcabValue)
        value2 = AcabValue.safe_make(value)
        self.assertIsInstance(value2, AcabValue)
        self.assertIsInstance(value2.value, str)


    def test_statement_to_simple_value(self):
        """ Check a statement can be downgraded to a value """
        value = BasicStatement("test")
        self.assertIsInstance(value, Instruction)
        basic = value.to_word()
        self.assertIsInstance(basic, AcabValue)




    # set_data, apply_patams/tags,


    def test_value_copy(self):
        """ Check a value can be copied """
        value = AcabValue("test")
        value.tags.update(["a"])
        copied = value.copy()
        copied.tags.update(["b"])
        self.assertEqual(value, copied)
        self.assertNotEqual(value.uuid, copied.uuid)
        self.assertTrue("a" in copied.tags)
        self.assertFalse("b" in value.tags)


    def test_eq(self):
        """ Check values are equal by their name """
        val1 = AcabValue("test")
        val2 = AcabValue("test")
        self.assertEqual(val1, val2)

    def test_eq_by_id(self):
        """ Check a value is equal to itself """
        val1 = AcabValue("test")
        self.assertEqual(val1, val1)

    def test_eq_by_str(self):
        """ Check a value is equal to a string of its name """
        val1 = AcabValue("test")
        self.assertEqual(val1, "test")

    def test_eq_fail(self):
        """ Check two values are not equal if they have different names """
        val1 = AcabValue("test")
        val2 = AcabValue("blah")
        self.assertNotEqual(val1, val2)
