#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.abstract.query import QueryComponent, QueryOp
from py_rule import util
from py_rule.abstract.node import PyRuleNode



class QueryTests(unittest.TestCase):

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
        QueryOp.op_list['test'] = True
        comp = QueryComponent("test", [])
        self.assertIsInstance(comp, QueryComponent)
        del QueryOp.op_list['test']

    def test_var_set(self):
        QueryOp.op_list['test'] = True
        bind = PyRuleNode("an_input", data={util.BIND_S: True})
        comp = QueryComponent("test", [bind])
        var_set = comp.var_set
        self.assertTrue("an_input" in var_set['in'])
        del QueryOp.op_list['test']




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
