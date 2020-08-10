import unittest
import logging
from acab.working_memory.trie_wm.parsing import TransformParser as TP
from acab.abstract import transform
from acab.working_memory.trie_wm import util as KBU
from acab.abstract.bootstrap_parser import BootstrapParser
from acab.abstract.production_operator import ProductionOperator

class Trie_Transform_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        bp = BootstrapParser()
        TP.HOTLOAD_TRANS_OP << bp.query('operator.transform.n_ary.*',
                                        'operator.sugar')
        TP.HOTLOAD_TRANS_STATEMENTS << bp.query("operator.transform.statements.*",
                                                "operator.sugar")

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_ternary_operator(self):
        result = TP.parseString('$x \operator.transform.n_ary.regex /blah/ $a -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].op.pprint(), 'operator.transform.n_ary.regex')
        self.assertEqual(result.clauses[0]._params[0]._value, 'x')
        self.assertEqual(result.clauses[0]._params[1]._value,'blah')
        self.assertEqual(result.clauses[0]._params[2]._value, 'a')
        self.assertIsNotNone(result.clauses[0]._rebind)

    def test_ternary_operator_rebind(self):
        result = TP.parseString('$x \operator.transform.n_ary.regex /blah/ $awef -> $q')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].op.pprint(), 'operator.transform.n_ary.regex')
        self.assertEqual(result.clauses[0]._params[0].name, 'x')
        self.assertEqual(result.clauses[0]._params[1].name,'blah')
        self.assertEqual(result.clauses[0]._params[2].name, 'awef')
        self.assertEqual(result.clauses[0]._rebind.name, 'q')

    def test_unary_format(self):
        result = TP.parseString('\operator.transform.n_ary.format blah -> $y')
        self.assertEqual(result.clauses[0].op.pprint(), 'operator.transform.n_ary.format')


if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.Trie_Transform_Parser_Tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
