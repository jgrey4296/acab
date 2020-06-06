#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule import util
from py_rule.abstract.transform import TransformComponent, TransformOp
from py_rule.abstract.node import PyRuleNode
from py_rule.abstract.value import PyRuleValue as PV


class TransformTests(unittest.TestCase):

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
        TransformOp.op_dict['TestOp'] = True
        transform = TransformComponent("TestOp", [])
        self.assertIsInstance(transform, TransformComponent)
        del TransformOp.op_dict['TestOp']

    def test_var_set(self):
        TransformOp.op_dict['TestOp'] = True
        param = PV("input", data={util.BIND_S: True})
        outbind = PV("output", data={util.BIND_S: True})
        transform = TransformComponent("TestOp", [param], rebind=outbind)
        var_set = transform.var_set
        var_set_in_str = [x.name for x in var_set['in']]
        var_set_out_str = [x.name for x in var_set['out']]
        self.assertTrue("input" in var_set_in_str)
        self.assertTrue("output" in var_set_out_str)
        del TransformOp.op_dict['TestOp']


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
