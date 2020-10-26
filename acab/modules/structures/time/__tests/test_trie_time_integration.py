import unittest
import logging
from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.core.type_system import build_simple_type_system
from acab.abstract.core.sentence import Sentence
from acab.abstract.parsing import util as PU
from acab.modules.structures.time import util as TU
from acab.modules.structures.time.parsing import parser as tp
from acab.modules.structures.time.parsing import parser as tp
from acab.modules.structures.time.time_core import TimeContainer
from acab.working_memory.trie_wm.parsing import FactParser as fp

VALUE_TYPE_S = AcabConfig.Get()("Parsing.Structure", "VALUE_TYPE_S")

class TrieIntegrationTimeTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # setup class
        type_sys = build_simple_type_system()
        AcabValue._set_type_system(type_sys)
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
        fp.HOTLOAD_VALUES << tp.main_pattern

        a = fp.parseString("a.test.sentence.[[a b c $d]]")[0]
        self.assertIsInstance(a, Sentence)
        self.assertIsInstance(a[-1], TimeContainer)
        self.assertEqual(a[-1]._data[VALUE_TYPE_S], TU.TIME_PATTERN_S)

if __name__ == "__main__":
    #use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.TrieIntegrationTimeTests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
