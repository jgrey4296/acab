#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.core.sentence import Sentence
from acab.abstract.rule import action
from acab.abstract.rule import transform
from acab.abstract.rule.query import QueryComponent, QueryOp
from acab.abstract.rule.rule import Rule

from acab.modules.values import numbers
from acab.modules.values.numbers.parsing import NumberParser as NP
from acab.working_memory.trie_wm import util as KBU
from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import FactParser as FP
from acab.working_memory.trie_wm.parsing import QueryParser as QP
from acab.working_memory.trie_wm.parsing import RuleParser as RP
from acab.working_memory.trie_wm.parsing import TransformParser as TP
from acab.working_memory.trie_wm.trie_working_memory import TrieWM

class NumberRuleTests(unittest.TestCase):
    ns = None

    @classmethod
    def setUpClass(cls):
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
        self.assertIsInstance(result[0][-1].value, Rule)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_multiple_transforms(self):
        result = RP.parseString("a.rule: (::ρ)\nλoperator.transform.add $x 30 -> $y\nλoperator.transform.sub $y 20 -> $z\n\nend\n")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, Rule)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_multiple_transforms_on_single_line(self):
        result = RP.parseString("a.rule: (::ρ)\nλoperator.transform.add 20 -> $y, λoperator.transform.sub $y 20 -> $z\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, Rule)
        self.assertIsNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)


    def test_rule_with_query_transform_actions(self):
        result = RP.parseString("a.rule: (::ρ)\na.b.c?\n\nλoperator.transform.add $x 20 -> $y\n\nλoperator.action.add a.b.c\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][-1].value, Rule)
        self.assertIsNotNone(result[0][-1].value._query)
        self.assertIsNotNone(result[0][-1].value._transform)
        self.assertEqual(len(result[0][-1].value._action), 1)



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
