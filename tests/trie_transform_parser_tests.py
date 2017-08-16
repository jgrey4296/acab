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
            result = TP.parseString('($x + 20, $y + 5)')
            self.assertIsInstance(result, Transforms.Transform)
            self.assertEqual(len(result.components), 2)

      

if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Transform_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
