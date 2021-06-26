import unittest
# Setup root_logger:
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)
##############################

import acab
acab.setup()

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.parsing.TrieBootstrapper import TrieBootstrapper
from acab.abstract.core.values import Sentence
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionComponent, ProductionContainer

from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP

def S(*words):
    return Sentence.build(words)

class Trie_Action_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        root_logger.getLogger('').setLevel(root_logger.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = root_logger.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(root_logger.DEBUG)

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)

        logging = root_logger.getLogger(__name__)
        logging.root.addHandler(console)
        logging.root.addHandler(file_h)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_action_definition(self):
        test_str = "test: (::α)\nλoperator.add a.b.c\n\nend"
        definition = AP.action_definition.parseString(test_str)
        self.assertEqual(definition[0][-1].name, "test")

    def test_parse_action_no_params(self):
        test_str = "λoperator.add"

        result = AP.action_component.parseString(test_str)[0]
        self.assertIsInstance(result, ProductionComponent)

    # TODO test  sugar, test sentences, test values, test multi params
    @unittest.skip("TODO")
    def test_action_sugar(self):
        return


