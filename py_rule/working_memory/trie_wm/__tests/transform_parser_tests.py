import unittest
import logging
from py_rule.working_memory.trie_wm.parsing import TransformParser as TP
from py_rule.modules.operators.standard_operators import StandardOperators
from py_rule.abstract import transform
from py_rule.working_memory.trie_wm import util as KBU
from py_rule.abstract.bootstrap_parser import BootstrapParser
from py_rule.abstract.production_operator import ProductionOperator

class Trie_Transform_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        bp = BootstrapParser()
        os = StandardOperators()
        os.assert_parsers(bp)
        TP.UNARY_TRANS_OP << bp.query("operator.transform.unary.*",
                                      "operator.sugar")
        TP.BINARY_TRANS_OP << bp.query("operator.transform.binary.*",
                                       "operator.sugar")
        TP.TERNARY_TRANS_OP << bp.query("operator.transform.ternary.*",
                                        "operator.sugar")
        TP.HOTLOAD_TRANS_STATEMENTS << bp.query("operator.transform.statements.*",
                                                "operator.sugar")

        ProductionOperator.construct_subclass_tree()

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_ternary_operator(self):
        result = TP.parseString('$x RegexOp /blah/ $a -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].op, 'RegexOp')
        self.assertEqual(result.clauses[0]._params[0]._value, 'x')
        self.assertEqual(result.clauses[0]._params[1]._value,'blah')
        self.assertEqual(result.clauses[0]._params[2]._value, 'a')
        self.assertIsNotNone(result.clauses[0]._rebind)

    def test_ternary_operator_rebind(self):
        result = TP.parseString('$x RegexOp /blah/ $awef -> $q')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].op, 'RegexOp')
        self.assertEqual(result.clauses[0]._params[0].name, 'x')
        self.assertEqual(result.clauses[0]._params[1].name,'blah')
        self.assertEqual(result.clauses[0]._params[2].name, 'awef')
        self.assertEqual(result.clauses[0]._rebind.name, 'q')

    def test_unary_format(self):
        result = TP.parseString('FormatOp blah -> $y')
        self.assertEqual(result.clauses[0].op, 'FormatOp')


if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Transform_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
