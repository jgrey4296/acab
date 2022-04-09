import unittest
import logging as logmod
logging = logmod.getLogger(__name__)

import acab
acab.setup()

from acab.core.data.sentence import Sentence
from acab.core.parsing import parsers as PU
from acab.modules.structures.time import util as TU
from acab.modules.structures.time.parsing import parser as tp
from acab.modules.structures.time.parsing import parser as tp
from acab.modules.structures.time.time_core import TimeContainer
from acab.modules.parsing.exlo FactParser as fp

TYPE_INSTANCE_S = AcabConfig().value("Value.Structure", "TYPE_INSTANCE")

class TrieIntegrationTimeTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)

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

        a = fp.parse_string("a.test.sentence.[[a b c $d]]")[0]
        self.assertIsInstance(a, Sentence)
        self.assertIsInstance(a[-1], TimeContainer)
        self.assertEqual(a[-1]._data[TYPE_INSTANCE_S], TU.TIME_PATTERN_S)

