import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

import re

import acab
config = acab.setup()

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.parsing.TrieBootstrapper import TrieBootstrapper
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionContainer

from acab.modules.parsing.exlo.parsers import TransformParser as TP
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP

class Trie_Transform_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        # setup class
        bp = TrieBootstrapper()
        TP.HOTLOAD_TRANS_OP << bp.query('operator.ProductionContainer.*',
                                        'operator.sugar')
        TP.HOTLOAD_TRANS_STATEMENTS << bp.query("operator.ProductionContainer.statements.*",
                                                "operator.sugar")

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets

    # TODO test ProductionContainer sugar, test multi variables, test sentences, test values
    @unittest.skip("TODO")
    def test_transform_sugar(self):
        return

