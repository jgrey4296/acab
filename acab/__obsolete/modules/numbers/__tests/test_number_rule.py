#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging as logmod
logging = logmod.getLogger(__name__)


import acab
import warnings
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()

from acab.core.value.sentence import Sentence
from acab.core.value import action
from acab.core.value.instruction import ProductionOperator, ProductionStructure
from acab.core.util.sentences import ProductionComponent

from acab.modules.values import numbers
from acab.modules.values.numbers.parsing import NumberParser as NP
from acab.modules.parsing.exlo ActionParser as AP
from acab.modules.parsing.exlo FactParser as FP
from acab.modules.parsing.exlo QueryParser as QP
from acab.modules.parsing.exlo RuleParser as RP
from acab.modules.parsing.exlo TransformParser as TP

class NumberRuleTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

        NumberRuleTests.ns = numbers.MODULE()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

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
        result = RP.parse_string("a.rule: (::ρ)\nλoperator.transform.add $x 20 -> $y\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_multiple_transforms(self):
        result = RP.parse_string("a.rule: (::ρ)\nλoperator.transform.add $x 30 -> $y\nλoperator.transform.sub $y 20 -> $z\n\nend\n")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_multiple_transforms_on_single_line(self):
        result = RP.parse_string("a.rule: (::ρ)\nλoperator.transform.add 20 -> $y, λoperator.transform.sub $y 20 -> $z\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_query_transform_actions(self):
        result = RP.parse_string("a.rule: (::ρ)\na.b.c?\n\nλoperator.transform.add $x 20 -> $y\n\nλoperator.action.add a.b.c\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, ProductionStructure)
        self.assertIsNotNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)
        self.assertEqual(len(result[0][-1].value._action), 1)


