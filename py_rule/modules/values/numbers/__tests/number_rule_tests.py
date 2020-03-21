#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.modules.values.numbers.parsing import NumberParser as NP
from py_rule.working_memory.trie_wm.parsing import ActionParser as AP
from py_rule.working_memory.trie_wm.parsing import TransformParser as TP
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.working_memory.trie_wm.parsing import QueryParser as QP
from py_rule.working_memory.trie_wm.parsing import RuleParser as RP
from py_rule.abstract import action
from py_rule.abstract.comparison import Comparison, CompOp
from py_rule.abstract.sentence import Sentence
from py_rule.abstract import transform
from py_rule.abstract.rule import Rule
from py_rule import util
from py_rule.modules.operators.operator_module import OperatorSpec
from py_rule.modules.values.numbers.number_module import NumberSpecification
from py_rule.working_memory.trie_wm.trie_working_memory import TrieWM
from py_rule.working_memory.trie_wm import util as KBU


class NumberTests(unittest.TestCase):
    os = None
    ns = None

    @classmethod
    def setUpClass(cls):
        NumberTests.os = OperatorSpec()
        NumberTests.ns = NumberSpecification()

    def setUp(self):
        self.trie = TrieWM()
        self.trie.add_modules([NumberTests.os, NumberTests.ns])
        self.trie.build_operator_parser()

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass


    def test_rule_with_transform(self):
        result = RP.parseString("a.rule:\n$x AddOp 20 -> $y\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][1], Rule)
        self.assertIsNone(result[0][1]._query)
        self.assertIsNotNone(result[0][1]._transform)


    def test_rule_with_multiple_transforms(self):
        result = RP.parseString("a.rule:\n $x AddOp 20 -> $y, $y SubOp 20\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][1], Rule)
        self.assertIsNone(result[0][1]._query)
        self.assertIsNotNone(result[0][1]._transform)


    def test_rule_with_multiple_transforms_on_single_line(self):
        result = RP.parseString("a.rule:\n$x AddOp 20 -> $y,$y SubOp 20\n\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][1], Rule)
        self.assertIsNone(result[0][1]._query)
        self.assertIsNotNone(result[0][1]._transform)


    def test_rule_with_query_transform_actions(self):
        result = RP.parseString("a.rule:\na.b.c?\n\n$x AddOp 20\n\nActionAdd(a.b.c)\nend")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0][1], Rule)
        self.assertIsNotNone(result[0][1]._query)
        self.assertIsNotNone(result[0][1]._transform)
        self.assertEqual(len(result[0][1]._action), 1)



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
