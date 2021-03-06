#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.printing import util as PrU
from acab.abstract.core.value import AcabValue, AcabStatement
from acab.abstract.data.node import AcabNode
from acab.abstract.core.sentence import Sentence

AT_BIND_S = AcabConfig.Get()("Parsing.Structure", "AT_BIND_S")
BIND_S = AcabConfig.Get()("Parsing.Structure", "BIND_S")

class AcabValueTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_construction(self):
        value = AcabValue("test")
        self.assertIsInstance(value, AcabValue)

    def test_var_set(self):
        value = AcabValue("test")
        var_set = value.var_set
        self.assertIsInstance(var_set, dict)
        self.assertTrue("in" in var_set)
        self.assertTrue("out" in var_set)

    def test_var_set_with_vars(self):
        value = AcabValue("test")
        value._params = ["a","b","c"]
        var_set = value.var_set
        self.assertTrue(all([x in var_set['in'] for x in ["a","b","c"]]))

    def test_attach_statement(self):
        value = AcabStatement("test")
        value._tags.add('testval')
        sen = Sentence([AcabValue(x) for x in range(5)])
        self.assertEqual(sen[-1].value, 4)
        self.assertIsInstance(sen[-1].value, int)
        copied = sen.attach_statement(value)
        self.assertIsInstance(copied[-1], AcabStatement)
        self.assertEqual(copied[-1]._value, "test")
        self.assertEqual(copied[-1].name, 4)

    def test_attach_statement_with_tags(self):
        value = AcabStatement("test")
        value._tags.add('testval')
        sen = Sentence([AcabValue(x) for x in range(5)])
        self.assertEqual(sen[-1]._value, 4)
        self.assertIsInstance(sen[-1]._value, int)
        copied = sen.attach_statement(value)

        self.assertIsInstance(copied[-1], AcabStatement)

    def test_has_tag(self):
        value = AcabValue("test")
        value._tags.update(["a"])
        self.assertTrue(value.has_tag("a"))

    def test_has_tag_fail(self):
        value = AcabValue("test")
        value._tags.update(["a"])
        self.assertFalse(value.has_tag("q"))

    def test_has_tag_multi(self):
        value = AcabValue("test")
        value._tags.update(["a", "b", "c"])
        self.assertTrue(value.has_tag("a", "b", "c"))

    def test_has_tag_multi_fail(self):
        value = AcabValue("test")
        value._tags.update(["a", "b", "c"])
        self.assertFalse(value.has_tag("a", "b", "c", "q"))

    def test_pprint_at_var(self):
        value = AcabValue("test")
        value._data.update({BIND_S: AT_BIND_S})
        self.assertTrue(value.is_at_var)
        self.assertEqual(value.pprint(), "@test")

    @unittest.skip('TODO')
    def test_verify(self):
        return

    @unittest.skip("TODO")
    def test_verify_fail(self):
        return

    def test_safe_make(self):
        value = AcabValue.safe_make("test")
        self.assertIsInstance(value, AcabValue)
        value2 = AcabValue.safe_make(value)
        self.assertIsInstance(value2, AcabValue)
        self.assertIsInstance(value2._value, str)


    def test_statement_to_simple_value(self):
        value = AcabStatement("test")
        self.assertIsInstance(value, AcabStatement)
        basic = value.to_simple_value()
        self.assertIsInstance(basic, AcabValue)


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
