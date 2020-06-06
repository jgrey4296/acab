#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.abstract.query import QueryComponent, QueryOp
from py_rule import util
from py_rule.abstract.node import PyRuleNode
from py_rule.abstract.value import PyRuleValue as PV



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
        QueryOp.op_dict['test'] = True
        comp = QueryComponent("test", [])
        self.assertIsInstance(comp, QueryComponent)
        del QueryOp.op_dict['test']

    def test_var_set(self):
        QueryOp.op_dict['test'] = True
        bind = PV("an_input", data={util.BIND_S: True})
        comp = QueryComponent("test", [bind])
        var_set = comp.var_set
        var_set_str = [x.name for x in var_set['in']]
        self.assertTrue("an_input" in var_set_str)
        del QueryOp.op_dict['test']




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
