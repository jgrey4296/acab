import unittest
import logging
from test_context import py_rule
from py_rule.trie.parsing import TransformParser as TP
from py_rule.abstract import transforms

class Trie_Transform_Parser_Tests(unittest.TestCase):

      def setUp(self):
            return 1

      def tearDown(self):
            return 1

      #----------
      #use testcase snippets
      def test_basic_transform_core(self):
            result = TP.transform_core.parseString('$x + 20')[0]
            self.assertIsInstance(result, transforms.OperatorTransform)
            self.assertIsInstance(result._op, transforms.TransformOp)
            self.assertEqual(len(result._params), 2)

      def test_basic_transform_core_rebind(self):
            result = TP.transform_core.parseString('$y * 20 -> $z')[0]
            self.assertIsInstance(result, transforms.OperatorTransform)
            self.assertIsInstance(result._op, transforms.TransformOp)
            self.assertEqual(result._params[0]._value, "y")
            self.assertTrue(result._params[0]._data['bind'])
            self.assertEqual(result._params[1]._value, 20)
            self.assertIsNotNone(result._rebind)
            self.assertEqual(result._rebind._value, 'z')

      def test_basic_transform(self):
            result = TP.parseString('$x + 20, $y + 5')
            self.assertIsInstance(result, transforms.Transform)
            self.assertEqual(len(result._components), 2)


      def test_unary_operator(self):
            result = TP.parseString('-$x')
            self.assertIsInstance(result, transforms.Transform)
            self.assertEqual(len(result._components), 1)
            self.assertIsInstance(result._components[0]._op, transforms.TransformOp)
            self.assertEqual(result._components[0]._params[0]._value, "x")
            self.assertIsNone(result._components[0]._rebind)

      def test_unary_rebind(self):
            result = TP.parseString('-$x -> $y')
            self.assertIsInstance(result, transforms.Transform)
            self.assertEqual(len(result._components), 1)
            self.assertIsInstance(result._components[0]._op, transforms.TransformOp)
            self.assertEqual(result._components[0]._params[0]._value, "x")
            self.assertIsNotNone(result._components[0]._rebind)
            self.assertEqual(result._components[0]._rebind._value, 'y')

      def test_binary_operator(self):
            result = TP.parseString('$x + 20')
            self.assertIsInstance(result, transforms.Transform)
            self.assertEqual(len(result._components), 1)
            self.assertIsInstance(result._components[0]._op, transforms.TransformOp)
            self.assertEqual(result._components[0]._params[0]._value, 'x')
            self.assertEqual(result._components[0]._params[1]._value, 20)
            self.assertIsNone(result._components[0]._rebind)

      def test_binary_rebind(self):
            result = TP.parseString('$x + 20 -> $y')
            self.assertIsInstance(result, transforms.Transform)
            self.assertEqual(len(result._components), 1)
            self.assertIsInstance(result._components[0]._op, transforms.TransformOp)
            self.assertEqual(result._components[0]._params[0]._value, 'x')
            self.assertEqual(result._components[0]._params[1]._value, 20)
            self.assertEqual(result._components[0]._rebind._value, 'y')

      def test_ternary_operator(self):
            result = TP.parseString('$x ~= /blah/ $a')
            self.assertIsInstance(result, transforms.Transform)
            self.assertEqual(len(result._components), 1)
            self.assertIsInstance(result._components[0]._op, transforms.TransformOp)
            self.assertEqual(result._components[0]._params[0]._value, 'x')
            self.assertEqual(result._components[0]._params[1],'blah')
            self.assertEqual(result._components[0]._params[2]._value, 'a')
            self.assertIsNone(result._components[0]._rebind)

      def test_ternary_operator_rebind(self):
            result = TP.parseString('$x ~= /blah/ $awef -> $q')
            self.assertIsInstance(result, transforms.Transform)
            self.assertEqual(len(result._components), 1)
            self.assertIsInstance(result._components[0]._op, transforms.TransformOp)
            self.assertEqual(result._components[0]._params[0]._value, 'x')
            self.assertEqual(result._components[0]._params[1],'blah')
            self.assertEqual(result._components[0]._params[2]._value, 'awef')
            self.assertEqual(result._components[0]._rebind._value, 'q')


      def test_binary_rand_operator(self):
            result = TP.parseString('$x <-> $y')
            self.assertEqual(len(result._components), 1)
            self.assertIsInstance(result._components[0]._op, transforms.RandOp)

      def test_unary_round(self):
            result = TP.parseString('_$x')
            self.assertIsInstance(result._components[0]._op, transforms.RoundOp)


      def test_selection_transform_all(self):
            result = TP.select.parseString('select _ - _')[0]
            self.assertIsInstance(result, transforms.SelectionTransform)
            self.assertEqual(result._lBound._value, "_")
            self.assertEqual(result._uBound._value, "_")

      def test_selection_transform_bounded(self):
            result = TP.select.parseString('select 0 - 2')[0]
            self.assertIsInstance(result, transforms.SelectionTransform)
            self.assertEqual(result._lBound._value, 0)
            self.assertEqual(result._uBound._value, 2)

      def test_selection_transform_variable(self):
            result = TP.select.parseString('select $x - $y')[0]
            self.assertIsInstance(result, transforms.SelectionTransform)
            self.assertEqual(result._lBound._value, 'x')
            self.assertEqual(result._uBound._value, 'y')

      def test_fact_str_equal(self):
            transforms = ["$x + 20", "$x + 20, $y + 5",
                          "$xc - 10", "$x * 100",
                          "$x + 20 -> $y",
                          "$Blah + $bloo -> $BLEE",
                          "-$x", "_$x", "-$x -> $y",
                          "_$x -> $y", "$x ~= /blah/$a",
                          "$x ~= /aw+/$b -> $blah",
                          "$x + 2d5",
                          "select 0 - 2",
                          "select _ - _",
                          "select $x - $y"
            ]
            parsed = [TP.parseString(x) for x in transforms]
            zipped = zip(transforms, parsed)
            for t,p in zipped:
                  self.assertEqual(t,str(p))





if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Transform_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
