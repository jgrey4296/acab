#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging

from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.sentence import Sentence

def S(*values):
    return Sentence([PyRuleValue(x) for x in values])

class SentenceTests(unittest.TestCase):

    def test_construction(self):
        val = S("a","test","value")
        self.assertIsInstance(val, Sentence)
        self.assertIsInstance(val, PyRuleValue)

    def test_length(self):
        val = S("a","test","value")
        self.assertEqual(len(val), 3)

    def test_eq(self):
        val = S("a","test","value")
        val2 = S("a","test","value")
        self.assertEqual(val, val2)
        self.assertEqual(val, val)

    def test_iter(self):
        val = S("a","test","value")
        for x,y in zip(val, ["a","test","value"]):
            self.assertEqual(x._value, y)

    def test_get_item(self):
        val = S("a","test","value")
        self.assertIsInstance(val[0], PyRuleValue)
        self.assertEqual(val[0]._value, "a")
        self.assertEqual(val[1]._value, "test")
        self.assertEqual(val[2]._value, "value")


    @unittest.skip("TODO")
    def test_bind(self):
        return

    def test_copy(self):
        val = S("a","test","value")
        val2 = val.copy()
        self.assertEqual(val, val2)

    def test_add(self):
        val = S("a","test","value")
        val2 = S("additional", "sentence")
        val3 = val.add(val2)

        self.assertIsInstance(val, Sentence)
        self.assertIsInstance(val2, Sentence)
        self.assertIsInstance(val3, Sentence)

        for x,y in zip(val3, ["a","test","value","additional","sentence"]):
            self.assertEqual(x._value, y)


    def test_slice(self):
        val = S("a","test","value")
        self.assertIsInstance(val[1:], Sentence)
        for x,y in zip(val[1:], ["test", "value"]):
            self.assertIsInstance(x, PyRuleValue)
            self.assertEqual(x._value, y)

    @unittest.skip("TODO")
    def test_attach_statement(self):
        pass

    @unittest.skip("TODO")
    def test_detach_statement(self):
        pass




if __name__ == "__main__":
    #run python $filename to use this logging setup
    #using python -m unittest $filename won't
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
