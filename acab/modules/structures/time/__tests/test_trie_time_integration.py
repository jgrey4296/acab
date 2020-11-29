import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

from acab.abstract.core.values import Sentence
from acab.abstract.parsing import parsers as PU
from acab.modules.structures.time import util as TU
from acab.modules.structures.time.parsing import parser as tp
from acab.modules.structures.time.parsing import parser as tp
from acab.modules.structures.time.time_core import TimeContainer
from acab.working_memory.trie_wm.parsing import FactParser as fp

TYPE_INSTANCE_S = AcabConfig.Get().value("Value.Structure", "TYPE_INSTANCE")

class TrieIntegrationTimeTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        #Hotload value and bind
        tp.HOTLOAD_VALUE << PU.BASIC_VALUE
        tp.HOTLOAD_BIND << PU.BIND

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_basic_parser_extension(self):
        PU.HOTLOAD_VALUES << tp.main_pattern

        a = fp.parseString("a.test.sentence.[[a b c $d]]")[0]
        self.assertIsInstance(a, Sentence)
        self.assertIsInstance(a[-1], TimeContainer)
        self.assertEqual(a[-1]._data[TYPE_INSTANCE_S], TU.TIME_PATTERN_S)

