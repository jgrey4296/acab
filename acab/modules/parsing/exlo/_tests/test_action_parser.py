import unittest
# Setup root_logger:
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)
##############################

import acab
acab.setup()

from acab.core.data.values import AcabValue
from acab.core.data.values import Sentence
from acab.core.parsing.trie_bootstrapper import TrieBootstrapper
from acab.core.data.values import Sentence
from acab.core.data.production_abstractions import ProductionOperator, ProductionComponent, ProductionContainer
from acab.core.data import default_structure as DS
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

    #----------
    #use testcase snippets
    def test_action_definition(self):
        test_str = "test:\n  λoperator.add a.b.c\nend"
        definition = AP.action_definition.parseString(test_str)[0]
        self.assertEqual(definition.name, "test")

    def test_parse_action_no_params(self):
        test_str = "λoperator.add"

        result = AP.action_component.parseString(test_str)[0]
        self.assertIsInstance(result, ProductionComponent)

    def test_multi_var_action(self):
        result = AP.action_component.parseString("λa.b.c $x $y $z")[0]
        self.assertIsInstance(result, ProductionComponent)
        self.assertEqual(len(result.params), 3)
        self.assertTrue(all([isinstance(x, AcabValue) for x in result.params]))

    def test_actions(self):
        result = AP.actions.parseString("  λa.b.c\n  a.b.d\n  λa.b.c blah bloo")[0]
        self.assertIsInstance(result, ProductionContainer)
        self.assertEqual(len(result.clauses), 3)

    # TODO test  sugar, test sentences, test values, test multi params
    @unittest.skip("TODO")
    def test_action_sugar(self):
        return


