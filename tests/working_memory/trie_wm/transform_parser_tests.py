import unittest
import logging
from test_context import py_rule
from py_rule.working_memory.trie_wm.parsing import TransformParser as TP
from py_rule.modules.standard_operators.operator_module import OperatorSpec
from py_rule.modules.standard_operators import transforms
from py_rule.abstract import transform
from py_rule.working_memory.trie_wm import util as KBU

class Trie_Transform_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        os = OperatorSpec()
        os._construct_comp_ops()
        os._construct_action_ops()
        os._construct_transform_ops()
        TP.build_operators()

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_unary_operator(self):
        result = TP.parseString('-$x')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._components), 1)
        self.assertIsInstance(result._components[0]._op, transform.TransformOp)
        self.assertEqual(result._components[0]._params[0]._value, "x")
        self.assertIsNone(result._components[0]._rebind)

    def test_unary_rebind(self):
        result = TP.parseString('-$x -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._components), 1)
        self.assertIsInstance(result._components[0]._op, transform.TransformOp)
        self.assertEqual(result._components[0]._params[0]._value, "x")
        self.assertIsNotNone(result._components[0]._rebind)
        self.assertEqual(result._components[0]._rebind._value, 'y')

    def test_ternary_operator(self):
        result = TP.parseString('$x ~= /blah/ $a')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._components), 1)
        self.assertIsInstance(result._components[0]._op, transform.TransformOp)
        self.assertEqual(result._components[0]._params[0]._value, 'x')
        self.assertEqual(result._components[0]._params[1]._value,'blah')
        self.assertEqual(result._components[0]._params[2]._value, 'a')
        self.assertIsNone(result._components[0]._rebind)

    def test_ternary_operator_rebind(self):
        result = TP.parseString('$x ~= /blah/ $awef -> $q')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._components), 1)
        self.assertIsInstance(result._components[0]._op, transform.TransformOp)
        self.assertEqual(result._components[0]._params[0]._value, 'x')
        self.assertEqual(result._components[0]._params[1]._value,'blah')
        self.assertEqual(result._components[0]._params[2]._value, 'awef')
        self.assertEqual(result._components[0]._rebind._value, 'q')

    def test_binary_rand_operator(self):
        result = TP.parseString('$x <-> $y')
        self.assertEqual(len(result._components), 1)
        self.assertIsInstance(result._components[0]._op, transforms.RandOp)

    def test_unary_round(self):
        result = TP.parseString('_$x')
        self.assertIsInstance(result._components[0]._op, transforms.RoundOp)

    def test_unary_format(self):
        result = TP.parseString('~{} blah')
        self.assertIsInstance(result._components[0]._op, transforms.FormatOp)


if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Transform_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
