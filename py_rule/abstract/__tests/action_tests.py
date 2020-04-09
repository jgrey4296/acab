#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule import abstract
from py_rule.abstract.node import PyRuleNode
from py_rule.abstract.action import ActionComponent
from py_rule import util
from py_rule.abstract.sentence import Sentence


class ActionTests(unittest.TestCase):

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
        action = ActionComponent("testOp", [])
        self.assertIsInstance(action, ActionComponent)

    def test_var_set(self):
        param = PyRuleNode("test", data={util.BIND_S: True})
        action = ActionComponent("testOp", [Sentence([param])])
        var_set = action.var_set
        self.assertTrue("test" in var_set['in'])

    @unittest.skip("TODO")
    def test_verify(self):
        # create an action with numerous action components,
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
