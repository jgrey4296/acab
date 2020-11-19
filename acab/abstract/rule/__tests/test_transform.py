#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue as PV
from acab.abstract.data.node import AcabNode
from acab.abstract.rule.transform import TransformComponent, TransformOp

BIND_S = AcabConfig.Get().value("Value.Structure", "BIND")

class TransformTests(unittest.TestCase):

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
        transform = TransformComponent(Sentence.build(["TestOp"]), [])
        self.assertIsInstance(transform, TransformComponent)

    def test_var_set(self):
        param = PV("input", data={BIND_S: True})
        outbind = PV("output", data={BIND_S: True})
        transform = TransformComponent(Sentence.build(["TestOp"]), [param], rebind=outbind)
        var_set = transform.var_set
        var_set_in_str = [x.name for x in var_set['in']]
        var_set_out_str = [x.name for x in var_set['out']]
        self.assertTrue("input" in var_set_in_str)
        self.assertTrue("output" in var_set_out_str)


