#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import acab

config = acab.setup()

import acab.core.value.default_structure as DS
from acab.core.data.node import AcabNode
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.interfaces.value import Value_i

AT_BIND_S = DS.AT_BIND
BIND_S    = DS.BIND

# Explicit, instead of ValueFactory

class AcabValueTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

    #----------
    #use testcase snippets
    def test_build(self):
        """ Check a value can be created safely with "build" """
        value = AcabValue("test",
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

    def test_has_tag(self):
        """ Check a sentence can report whether it has a tag """
        value = AcabValue("test", tags=["a"])
        self.assertTrue(value.has_tag(AcabValue("a")))

    def test_has_tag_fail_on_str(self):
        """ check a value can report it doesn't have a tag """
        value = AcabValue("test", tags=["a"])
        self.assertFalse(value.has_tag("a"))

    def test_has_tag_multi(self):
        """ Check a value can report it has multiple tags """
        value = AcabValue("test", tags=["a", "b", "c"])
        self.assertTrue(value.has_tag(*[AcabValue(x) for x in ["a", "b", "c"]]))

    def test_has_tag_multi_fail(self):
        """ Check a value can report it doesn't have all specified tags """
        value = AcabValue("test", tags=["a", "b", "c"])
        self.assertFalse(value.has_tag(*[AcabValue(x) for x in ["a", "b", "c", "q"]]))

    def test_build(self):
        """ Check a value doesn't build to contain a value """
        value = AcabValue("test")
        self.assertIsInstance(value, AcabValue)
        value2 = AcabValue(value)
        self.assertIsInstance(value2, AcabValue)
        self.assertIsInstance(value2.value, str)


    def test_value_copy(self):
        """ Check a value can be copied """
        value = AcabValue("test", tags=["a"])
        copied = value.copy(tags=["b"])
        self.assertEqual(value, copied)
        self.assertNotEqual(value.uuid, copied.uuid)
        self.assertTrue(AcabValue("a") in copied.tags)
        self.assertFalse(AcabValue("b") in value.tags)


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

    def test_value_type_extension(self):
        with self.assertRaises(TypeError):
            AcabValue(2)

        AcabValue.extend_core(int)
        val = AcabValue(2)
        self.assertIsInstance(val, Value_i)
