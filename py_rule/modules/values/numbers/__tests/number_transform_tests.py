#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from py_rule.modules.values.numbers.parsing import NumberParser as NP
from py_rule.working_memory.trie_wm.parsing import ActionParser as AP
from py_rule.working_memory.trie_wm.parsing import TransformParser as TP
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.working_memory.trie_wm.parsing import QueryParser as QP
from py_rule.abstract import action
from py_rule.abstract.comparison import Comparison, CompOp
from py_rule.abstract.sentence import Sentence
from py_rule.abstract import transform
from py_rule import util
from py_rule.modules.operators.operator_module import OperatorSpec
from py_rule.modules.values.numbers.number_module import NumberSpecification
from py_rule.working_memory.trie_wm.trie_working_memory import TrieWM
from py_rule.working_memory.trie_wm import util as KBU


class NumberTransformTests(unittest.TestCase):
    os = None
    ns = None

    @classmethod
    def setUpClass(cls):
        NumberTransformTests.os = OperatorSpec()
        NumberTransformTests.ns = NumberSpecification()

    def setUp(self):
        self.trie = TrieWM()
        self.trie.add_modules([NumberTransformTests.os, NumberTransformTests.ns])
        self.trie.build_operator_parser()

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass


    def test_basic_transform_core(self):
        result = TP.transform_core.parseString('$x AddOp 20 -> $y')[0]
        self.assertIsInstance(result, transform.TransformComponent)
        self.assertEqual(result._op, "AddOp")
        self.assertEqual(len(result._params), 2)


    def test_basic_transform_core_rebind(self):
        result = TP.transform_core.parseString('$y MulOp 20 -> $z')[0]
        self.assertIsInstance(result, transform.TransformComponent)
        self.assertEqual(result._op, "MulOp")
        self.assertEqual(result._params[0]._value, "y")
        self.assertTrue(result._params[0]._data[KBU.BIND_S])
        self.assertEqual(result._params[1]._value, 20)
        self.assertIsNotNone(result._rebind)
        self.assertEqual(result._rebind._value, 'z')


    def test_basic_transform(self):
        result = TP.parseString('$x AddOp 20 -> $y, $y AddOp 5 -> $z')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._clauses), 2)


    def test_binary_operator(self):
        result = TP.parseString('$x AddOp 20 -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._clauses), 1)
        self.assertEqual(result._clauses[0]._op, "AddOp")
        self.assertEqual(result._clauses[0]._params[0]._value, 'x')
        self.assertEqual(result._clauses[0]._params[1]._value, 20)
        self.assertIsNotNone(result._clauses[0]._rebind)


    def test_binary_rebind(self):
        result = TP.parseString('$x AddOp 20 -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._clauses), 1)
        self.assertEqual(result._clauses[0]._op, "AddOp")
        self.assertEqual(result._clauses[0]._params[0]._value, 'x')
        self.assertEqual(result._clauses[0]._params[1]._value, 20)
        self.assertEqual(result._clauses[0]._rebind._value, 'y')

    def test_unary_round(self):
        result = TP.parseString('RoundOp $x -> $y')
        self.assertEqual(result._clauses[0]._op, 'RoundOp')

    def test_binary_rand_operator(self):
        result = TP.parseString('$x RandOp $y -> $z')
        self.assertEqual(len(result._clauses), 1)
        self.assertEqual(result._clauses[0]._op, 'RandOp')

    def test_unary_operator(self):
        result = TP.parseString('NegOp $x -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._clauses), 1)
        self.assertEqual(result._clauses[0]._op, "NegOp")
        self.assertEqual(result._clauses[0]._params[0]._value, "x")
        self.assertIsNotNone(result._clauses[0]._rebind)

    def test_unary_rebind(self):
        result = TP.parseString('NegOp $x -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._clauses), 1)
        self.assertEqual(result._clauses[0]._op, "NegOp")
        self.assertEqual(result._clauses[0]._params[0]._value, "x")
        self.assertIsNotNone(result._clauses[0]._rebind)
        self.assertEqual(result._clauses[0]._rebind._value, 'y')



    def test_fact_str_equal(self):
        transforms = ["$x AddOp 20 -> $y", "$x AddOp 20 -> $y\n$y AddOp 5 -> $z",
                      "$xc SubOp 10 -> $y", "$x MulOp 100 -> $y",
                      "$x AddOp 20 -> $y",
                      "$Blah AddOp $bloo -> $BLEE",
                      "NegOp $x -> $y", "RoundOp $x -> $y", "NegOp $x -> $y",
                      "RoundOp $x -> $y", "$x RegexOp /blah/$a -> $z",
                      "$x RegexOp /awAddOp/$b -> $blah",
                      "$x AddOp 2d5 -> $y"
        ]
        parsed = [TP.parseString(x) for x in transforms]
        zipped = zip(transforms, parsed)
        for rt,pt in zipped:
            self.assertEqual(rt,str(pt))



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
