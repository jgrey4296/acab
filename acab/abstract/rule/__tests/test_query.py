#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

from acab.abstract.rule.query import QueryComponent, QueryOp
from acab.abstract.data.node import AcabNode
from acab.abstract.core.value import AcabValue as PV
from acab.abstract.core.sentence import Sentence

BIND_S = AcabConfig.Get().value("Value.Structure", "BIND")

class QueryTests(unittest.TestCase):

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
        comp = QueryComponent(Sentence.build(["test"]), [])
        self.assertIsInstance(comp, QueryComponent)

    def test_var_set(self):
        bind = PV("an_input", data={BIND_S: True})
        comp = QueryComponent(Sentence.build(["test"]), [bind])
        var_set = comp.var_set
        var_set_str = [x.name for x in var_set['in']]
        self.assertTrue("an_input" in var_set_str)




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
