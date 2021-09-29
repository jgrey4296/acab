import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

import re

import acab
config = acab.setup()

from acab.core.data.values import AcabValue
from acab.core.data.values import Sentence
from acab.core.parsing.trie_bootstrapper import TrieBootstrapper
from acab.core.data.production_abstractions import ProductionOperator, ProductionContainer, ProductionComponent

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

    #----------
    def test_transform_core(self):
        """ Check a transform can be parsed """
        result = TP.transform_core.parseString("λa.b.c $x $y -> $z")[0]
        self.assertIsInstance(result, ProductionComponent)
        self.assertEqual(result.rebind.name, "z")

    def test_transforms(self):
        """ Check multiple transforms can be parsed """
        result = TP.transforms.parseString("  λa.b.c $x -> $y\n  λa.b.d $x -> $z")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 2)
