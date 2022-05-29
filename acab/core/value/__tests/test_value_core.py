#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import warnings

import acab
from acab import types as AT

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

import acab.core.defaults.value_keys as DS
from acab.core.data.node import AcabNode
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
import acab.interfaces.value as VI
from acab.interfaces.value import ValueFactory as VF

AT_BIND_S = DS.AT_BIND
BIND_S    = DS.BIND

# Explicit, instead of ValueFactory

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
        """
        Check the acceptible types for AcabValue can be extended.
        float is used instead of int, because if using the StringCacheValueMeta,
        int may already have been added
        """
        # Can't do contain check for type union
        self.assertNotEqual(float | AT.ValueCore, AT.ValueCore)
        with self.assertRaises(TypeError):
            AcabValue(2.5)

        AcabValue.extend_core(float)
        self.assertEqual(float | AT.ValueCore, AT.ValueCore)
        val = AcabValue(2.5)
        self.assertIsInstance(val, VI.Value_i)


    def test_to_sentences(self):
        val = AcabValue("Test")
        as_sens = val.to_sentences()
        self.assertIsInstance(as_sens, list)
        self.assertEqual(len(as_sens), 1)
        self.assertEqual(len(as_sens[0]), 1)
        self.assertIsInstance(as_sens[0], VI.Sentence_i)
        self.assertEqual(as_sens[0], "_:Test")

    def test_to_sentences_var(self):
        val = AcabValue("Test", data={DS.BIND: True})
        as_sens = val.to_sentences()
        self.assertIsInstance(as_sens, list)
        self.assertEqual(len(as_sens), 1)
        self.assertEqual(len(as_sens[0]), 1)
        self.assertIsInstance(as_sens[0], VI.Sentence_i)
        self.assertEqual(as_sens[0], "_:Test")
        self.assertTrue(as_sens[0].has_var)

    def test_value_params(self):
        val = AcabValue("test", params=["a", "b", "c"])
        self.assertTrue(all([isinstance(x, AcabValue) for x in val.params]))
        self.assertTrue(all([x.is_var for x in val.params]))

