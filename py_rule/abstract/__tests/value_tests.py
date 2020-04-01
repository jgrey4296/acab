#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.abstract.value import PyRuleValue

class PyRuleValueTests(unittest.TestCase):

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
        value = PyRuleValue("test")
        self.assertIsInstance(value, PyRuleValue)

    def test_var_set(self):
        value = PyRuleValue("test")
        var_set = value.var_set()
        self.assertIsInstance(var_set, dict)
        self.assertTrue("in" in var_set)
        self.assertTrue("out" in var_set)

    def test_var_set_with_vars(self):
        value = PyRuleValue("test")
        value._vars = ["a","b","c"]
        var_set = value.var_set()
        self.assertTrue(all([x in var_set['in'] for x in ["a","b","c"]]))

    @unittest.skip("TODO")
    def test_apply_onto(self):
        return

    @unittest.skip('TODO')
    def test_has_tag(self):
        return


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
