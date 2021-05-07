#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as root_logger
logging = root_logger.getLogger(__name__)


import acab
acab.setup()

from acab.abstract.core.values import Sentence
from acab.abstract.core.import action
from acab.abstract.core.production_abstractions import ProductionComponent, ProductionOperator, ProductionStructure

from acab.modules.values import numbers
from acab.modules.values.numbers.parsing import NumberParser as NP
from acab.working_memory.trie_wm import util as KBU
from acab.modules.parsing.el_parsing ActionParser as AP
from acab.modules.parsing.el_parsing FactParser as FP
from acab.modules.parsing.el_parsing QueryParser as QP
from acab.modules.parsing.el_parsing RuleParser as RP
from acab.modules.parsing.el_parsing TransformParser as TP
from acab.working_memory.trie_wm.trie_working_memory import TrieWM

class NumberRuleTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        NumberRuleTests.ns = numbers.MODULE()

    def setUp(self):
        self.trie = TrieWM()
        self.trie.construct_parsers_from_fragments([NumberRuleTests.ns])

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass


    def test_rule_with_transform(self):
        result = RP.parseString("a.rule: (::ρ)\nλoperator.transform.add $x 20 -> $y\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_multiple_transforms(self):
        result = RP.parseString("a.rule: (::ρ)\nλoperator.transform.add $x 30 -> $y\nλoperator.transform.sub $y 20 -> $z\n\nend\n")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_multiple_transforms_on_single_line(self):
        result = RP.parseString("a.rule: (::ρ)\nλoperator.transform.add 20 -> $y, λoperator.transform.sub $y 20 -> $z\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_query_transform_actions(self):
        result = RP.parseString("a.rule: (::ρ)\na.b.c?\n\nλoperator.transform.add $x 20 -> $y\n\nλoperator.action.add a.b.c\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNotNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)
        self.assertEqual(len(result[0][-1].value._action), 1)



