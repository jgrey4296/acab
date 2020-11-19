#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

from acab import abstract
from acab.abstract.data.node import AcabNode
from acab.abstract.core.value import AcabValue as AV
from acab.abstract.rule.action import ActionComponent, ActionOp
from acab.abstract.core.sentence import Sentence

BIND_S = AcabConfig.Get().value("Value.Structure", "BIND")

class ActionTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

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




