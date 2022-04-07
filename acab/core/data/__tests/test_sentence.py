#https://docs.python.org/3/library/unittest.html
import logging as logmod
import unittest
from os import listdir
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import acab

config = acab.setup()

from acab.core.data.default_structure import BIND
from acab.core.data.value import AcabValue
from acab.core.data.sentence import Sentence
from acab.interfaces.value import Sentence_i, Value_i
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser

dsl = None

class SentenceTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)

        global dsl
        # Set up the parser to ease test setup
        dsl   = ppDSL.PyParseDSL([], [], [])
        dsl.register(EXLO_Parser)
        dsl.build()
        # dsl()

    def test_construction(self):
        """ check simple sentence construction """
        val = Sentence(["a","test","value"])
        self.assertIsInstance(val, Sentence_i)
        self.assertIsInstance(val, Value_i)

    def test_length(self):
        """ check simple sentence length """
        val = Sentence(["a","test","value"])
        self.assertEqual(len(val), 3)

    def test_eq(self):
        """ check two sentences of equal values are equal """
        val = Sentence(["a","test","value"])
        val2 = Sentence(["a","test","value"])
        self.assertEqual(val, val2)
        self.assertEqual(val, val)

    def test_eq_fail(self):
        """ Check two sentences of different values are different """
        val = Sentence(["a", "test", "value"])
        val2 = Sentence(["a", "test", "difference"])
        self.assertNotEqual(val, val2)


    def test_build(self):
        """ Check the utility method "build" works """
        val = Sentence(name="_", value=[AcabValue("a"), AcabValue("test"), AcabValue("value")])
        val2 = Sentence(["a", "test","value"])
        self.assertEqual(val, val2)


    def test_iter(self):
        """ Check a sentence can be iterated over """
        val = Sentence(["a","test","value"])
        for x,y in zip(val, ["a","test","value"]):
            self.assertEqual(x.value, y)

    def test_get_item(self):
        """ Check a single value can be retrieved from a sentence """
        val = Sentence(["a","test","value"])
        self.assertIsInstance(val[0], Value_i)
        self.assertEqual(val[0].value, "a")
        self.assertEqual(val[1].value, "test")
        self.assertEqual(val[2].value, "value")

    def test_copy(self):
        """ Check a sentence can be copied """
        val = Sentence(["a","test","value"])
        val2 = val.copy()
        self.assertEqual(val, val2)
        self.assertNotEqual(val.uuid, val2.uuid)

    def test_copy_independence(self):
        """ Check a copied sentence is independent from its original """
        val = Sentence(["a","test","value"])
        val2 = val.copy()
        val.value.append(Sentence(["test"])[0])
        self.assertNotEqual(val, val2)

    def test_words_independence(self):
        """ Check using a sentence's `words` property provides a new list of words,
        not access to the sentence's internal word list
        """
        val = Sentence(["a","test","value"])
        val.words.append(Sentence(["test"])[0])
        self.assertEqual(val, "_:a.test.value")

    def test_copy_data_independence(self):
        """ Check a copied sentence's data is independent from its original """
        val = Sentence(["a","test","value"])
        val2 = val.copy()
        val.data.update({"blah" : "bloo"})
        self.assertFalse("blah" in val2.data)


    def test_add(self):
        """ Check a value can be added into a sentence, building a new sentence """
        val = Sentence(["a","test","value"])
        val2 = Sentence(["additional", "sentence"])
        val3 = val.add(val2)

        self.assertIsInstance(val, Sentence_i)
        self.assertIsInstance(val2, Sentence_i)
        self.assertIsInstance(val3, Sentence_i)

        self.assertNotEqual(val.uuid, val2.uuid)
        self.assertNotEqual(val2.uuid, val3.uuid)

        for x,y in zip(val3, ["a","test","value","additional","sentence"]):
            self.assertEqual(x.value, y)

    def test_add_flatten(self):
        """
        Check the default behaviour of Sentence.add is to flatten any sentence
        into the newly constructed one.
        """
        sen1 = dsl("a.test.sen")[0]
        sen2 = dsl("more.words.blah")[0]
        sen3 = sen1.add(sen2)

        self.assertTrue(all([not isinstance(x, Sentence) for x in sen3]))
        self.assertEqual(len(sen3), 6)

    def test_add_not_flatten(self):
        """
        Check that sentences can be added as a word without flattening using Sentence.add
        """
        sen1 = dsl("a.test.sen")[0]
        sen2 = dsl("more.words.blah")[0]
        sen3 = sen1.add([sen2])

        self.assertFalse(all([not isinstance(x, Sentence) for x in sen3]))
        self.assertEqual(len(sen3), 4)

    def test_internal_sentence(self):
        """
        Test Behaviour of a sentence as a word in another sentence
        """
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("another.sentence")[0]
        sen3 = sen1.add([sen2])

        self.assertEqual(sen1, "_:a.test.sentence")
        self.assertEqual(sen2, "_:another.sentence")
        self.assertEqual(sen3, "_:a.test.sentence.another.sentence")

        self.assertIsInstance(sen3.words[-1], Sentence_i)
        self.assertEqual(len(sen3.words[-1]), 2)

    @unittest.skip
    def test_bind(self):
        """ Check variables can be bound in a sentence, building a new sentence """
        val = Sentence(["a","test","value"])
        var = Sentence(["var"])
        var[0].data.update({BIND : True})
        sen = val.add(var)

        bound = sen.bind({"var" : "blah"})

        self.assertNotEqual(sen.uuid, bound.uuid)
        self.assertFalse(bound[-1].is_var)
        self.assertEqual(bound[-1].value, "blah")

    @unittest.skip
    def test_bind_nop(self):
        """ Check a sentence binding doesn't create a new sentence unless it has to """
        val = Sentence(["a","test","value"])
        var = Sentence(["var"])
        var[0].data.update({BIND: True})
        val[2].data.update({BIND : True})
        sen = val.add(var)

        bound = sen.bind({"not_var" : "blah"})

        self.assertEqual(sen,bound)
        self.assertTrue(bound[2].is_var)
        self.assertTrue(bound[-1].is_var)
        self.assertEqual(bound[-1].value, "var")

        # self.assertEqual(sen.uuid, bound.uuid)


    def test_get_item_slice(self):
        """ Check a subset of a sentence can be extracted as a new sentence """
        val = Sentence(["a","test","value"])
        self.assertIsInstance(val[1:], Sentence_i)
        for x,y in zip(val[1:], ["test", "value"]):
            self.assertIsInstance(x, Value_i)
            self.assertEqual(x.value, y)

    def test_attach_statement(self):
        """ Check a statement can be attached to the end of a sentence,
        taking the name of the last word """
        sen       = Sentence(["a","test","value"])
        to_attach = Sentence(["blah","bloo"])

        attached = sen.attach_statement(to_attach)
        self.assertNotEqual(sen, attached)
        self.assertEqual(sen[0:2], attached[0:2])
        self.assertTrue(all([x == y for x,y in zip(attached[-1].words, to_attach.words)]))
        self.assertEqual(attached[-1].name, "value")
        self.assertEqual(len(sen), len(attached))

    def test_detach_statement(self):
        """ Check a statement can be detached from a sentence,
        returning the last word to be a simple value """
        sen = Sentence(["a","test","value"])
        to_attach = Sentence(["blah","bloo"])
        attached = sen.attach_statement(to_attach)

        self.assertNotEqual(sen, attached)
        detached, statements = attached.detach_statement()

        self.assertEqual(detached, sen)
        self.assertEqual(statements[0][:], to_attach)
        self.assertEqual(len(sen), len(detached))

    def test_detach_complete(self):
        """ Check detaching a sentence detaches all statements from the entire sentence """
        sen = Sentence(["a","test","value"])
        to_attach = Sentence(["blah","bloo"])
        attached_first = sen.attach_statement(to_attach)

        sen2 = Sentence(["aweg"])
        second_attach = Sentence(["qwer", "qwop"])
        attached_second = sen2.attach_statement(second_attach)

        combined = attached_first.add(attached_second)
        combined_simple = Sentence(["a","test","value","aweg"])

        self.assertNotEqual(combined, combined_simple)

        detached, statements = combined.detach_statement()

        self.assertEqual(len(statements), 2)
        self.assertEqual(combined_simple, detached)


    def test_contains(self):
        """ Check a sentence can report if a word is contained in it """
        sen = Sentence(["a", "test", "sentence"])
        self.assertIn("test", sen)

    def test_contains_fail(self):
        """ Check a sentence can report a word is *not* in it """
        sen = Sentence(["a", "test", "sentence"])
        self.assertNotIn("blah", sen)

    def test_clear(self):
        """ Check a sentence can return an empty sentence """
        sen = Sentence(["a", "test", "sentence"])
        sen.data["test data"] = True
        sen_cleared = sen.clear()
        self.assertEqual(len(sen_cleared), 0)
        self.assertNotEqual(sen_cleared, sen)
        self.assertIn("test data", sen_cleared.data)

    def test_is_var_fail(self):
        """ Check a sentence is not a variable """
        sen = Sentence(["a", "test", "sentence"])
        self.assertFalse(sen.is_var)

    def test_is_var_fail_2(self):
        """ Check a sentence with a variable isn't a variable """
        sen = Sentence(["a", "test", "sentence"])
        sen[-1].data.update({BIND: True})
        self.assertTrue(sen[-1].is_var)
        self.assertFalse(sen.is_var)

    def test_is_var_pass(self):
        """ Check a sentence of a single word, that is a variable, is a variable """
        sen = Sentence(["single word"])
        sen[0].data.update({BIND: True})
        self.assertTrue(sen.is_var)

    def test_add(self):
        sen1 = Sentence(["a", "test", "sentence"])
        sen2 = Sentence(["blah"])
        sen3 = sen1.add(sen2)

        self.assertEqual(len(sen1), 3)
        self.assertEqual(len(sen2), 1)
        self.assertEqual(len(sen3), 4)
        self.assertIn(sen2[0], sen3)


    def test_dsl_build(self):
        sen1 = dsl("a.test.sentence")[0]
        self.assertIsInstance(sen1, Sentence_i)
