#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from acab import abstract
from acab.abstract.node import AcabNode
from acab.abstract.value import AcabValue as PV
from acab.abstract.action import ActionComponent, ActionOp
from acab import util
from acab.abstract.sentence import Sentence


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
        ActionOp.op_dict['testOp'] = True
        action = ActionComponent("testOp", [])
        self.assertIsInstance(action, ActionComponent)
        del ActionOp.op_dict['testOp']

    def test_var_set(self):
        ActionOp.op_dict['testOp'] = True
        param = PV("test", data={util.BIND_S: True})
        param_sen = Sentence([param])
        action = ActionComponent("testOp", [param_sen])
        var_set = action.var_set
        self.assertTrue(param in var_set['in'])
        del ActionOp.op_dict['testOp']

    @unittest.skip("TODO")
    def test_component_bind(self):
        pass

    @unittest.skip("TODO")
    def test_to_sentence(self):
        pass

    @unittest.skip("TODO")
    def test_component_pprint(self):
        pass

    @unittest.skip("TODO")
    def test_container_bind(self):
        pass

    @unittest.skip("TODO")
    def test_container_to_sentences(self):
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
