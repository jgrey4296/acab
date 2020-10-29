#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab import abstract
from acab.abstract.data.node import AcabNode
from acab.abstract.core.value import AcabValue as AV
from acab.abstract.rule.action import ActionComponent, ActionOp
from acab.abstract.core.sentence import Sentence
from acab.abstract.core.type_system import build_simple_type_system

BIND_S = AcabConfig.Get()("Parsing.Structure", "BIND_S")

class ActionTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # setup class
        type_sys = build_simple_type_system()

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_construction(self):
        action = ActionComponent(Sentence.build(["testOp"]), [])
        self.assertIsInstance(action, ActionComponent)

    def test_var_set(self):
        param = AV("test", data={BIND_S: True})
        param_sen = Sentence([param])
        action = ActionComponent(Sentence.build(["testOp"]), [param_sen])
        var_set = action.var_set
        self.assertTrue(param in var_set['in'])

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
