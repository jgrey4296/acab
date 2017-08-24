import unittest
import logging
from test_context import pyRule
from pyRule.trie import TransformParser as TP
from pyRule import Transforms
from pyRule.utils import Bind

class Trie_Transform_Parser_Tests(unittest.TestCase):
      
      def setUp(self):
            return 1

      def tearDown(self):
            return 1

      #----------
      #use testcase snippets
      def test_basic_transform_core(self):
            result = TP.transform_core.parseString('$x + 20')[0]
            self.assertIsInstance(result, Transforms.TransformComponent)
            self.assertEqual(result.op, Transforms.TROP.ADD)
            self.assertIsInstance(result.source, Bind)
            self.assertEqual(result.source.value, 'x')
            self.assertEqual(result.val, 20)
            self.assertIsNone(result.bind)
            self.assertIsNone(result.rebind)

      def test_basic_transform_core_rebind(self):
            result = TP.transform_core.parseString('$y * 20 -> $z')[0]
            self.assertIsInstance(result, Transforms.TransformComponent)
            self.assertEqual(result.op, Transforms.TROP.MUL)
            self.assertEqual(result.source.value, 'y')
            self.assertEqual(result.val, 20)
            self.assertIsNone(result.bind)
            self.assertIsNotNone(result.rebind)
            self.assertEqual(result.rebind.value, 'z')

      def test_basic_transform_core_bind(self):
            result = TP.transform_core.parseString('$y * $z')[0]
            self.assertIsNotNone(result.bind)
            self.assertIsNone(result.val)
            self.assertEqual(result.bind.value, 'z')


      def test_basic_transform(self):
            result = TP.parseString('$x + 20, $y + 5')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 2)


      def test_unary_operator(self):
            result = TP.parseString('-$x')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 1)
            self.assertEqual(result.components[0].op, Transforms.TROP.NEG)
            self.assertEqual(result.components[0].source.value, "x")
            self.assertIsNone(result.components[0].val)
            self.assertIsNone(result.components[0].bind)
            self.assertIsNone(result.components[0].rebind)
            
      def test_unary_rebind(self):
            result = TP.parseString('-$x -> $y')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 1)
            self.assertEqual(result.components[0].op, Transforms.TROP.NEG)
            self.assertEqual(result.components[0].source.value, "x")
            self.assertIsNone(result.components[0].val)
            self.assertIsNone(result.components[0].bind)
            self.assertIsNotNone(result.components[0].rebind)
            self.assertEqual(result.components[0].rebind.value, 'y')

      def test_binary_operator(self):
            result = TP.parseString('$x + 20')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 1)
            self.assertEqual(result.components[0].op, Transforms.TROP.ADD)
            self.assertEqual(result.components[0].source.value, 'x')
            self.assertEqual(result.components[0].val, 20)
            self.assertIsNone(result.components[0].bind)
            self.assertIsNone(result.components[0].rebind)

      def test_binary_rebind(self):
            result = TP.parseString('$x + 20 -> $y')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 1)
            self.assertEqual(result.components[0].op, Transforms.TROP.ADD)
            self.assertEqual(result.components[0].source.value, 'x')
            self.assertEqual(result.components[0].val, 20)
            self.assertIsNone(result.components[0].bind)
            self.assertEqual(result.components[0].rebind.value, 'y')

      def test_ternary_operator(self):
            result = TP.parseString('$x ~= /blah/ $a')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 1)
            self.assertEqual(result.components[0].op, Transforms.TROP.REGEX)
            self.assertEqual(result.components[0].source.value, 'x')
            self.assertEqual(result.components[0].val,'blah')
            self.assertEqual(result.components[0].bind.value, 'a')
            self.assertIsNone(result.components[0].rebind)

      def test_ternary_operator_rebind(self):
            result = TP.parseString('$x ~= /blah/ $awef -> $q')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 1)
            self.assertEqual(result.components[0].op, Transforms.TROP.REGEX)
            self.assertEqual(result.components[0].source.value, 'x')
            self.assertEqual(result.components[0].val,'blah')
            self.assertEqual(result.components[0].rebind.value, 'q')
            self.assertEqual(result.components[0].bind.value, 'awef')


      def test_binary_rand_operator(self):
            result = TP.parseString('$x <-> $y')
            self.assertEqual(len(result.components), 1)
            self.assertEqual(result.components[0].op, Transforms.TROP.RAND)

      def test_unary_round(self):
            result = TP.parseString('_$x')
            self.assertEqual(result.components[0].op, Transforms.TROP.ROUND)

            
            
      def test_fact_str_equal(self):
            transforms = ["$x + 20", "$x + 20, $y + 5",
                          "$xc - 10", "$x * 100",
                          "$x + 20 -> $y",
                          "$Blah + $bloo -> $BLEE",
                          "-$x", "_$x", "-$x -> $y",
                          "_$x -> $y", "$x ~= /blah/ $a",
                          "$x ~= /aw+/ $b -> $blah",
                          "$x + 2d5"]
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
