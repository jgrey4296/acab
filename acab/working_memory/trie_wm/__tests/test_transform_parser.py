import unittest
import logging
import re

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.core.type_system import build_simple_type_system
from acab.abstract.engine.bootstrap_parser import BootstrapParser
from acab.abstract.rule import transform
from acab.abstract.rule.production_operator import ProductionOperator
from acab.working_memory.trie_wm import util as KBU
from acab.working_memory.trie_wm.parsing import TransformParser as TP

class Trie_Transform_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # setup class
        type_sys = build_simple_type_system()
        bp = BootstrapParser()
        TP.HOTLOAD_TRANS_OP << bp.query('operator.transform.*',
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
        result = TP.parseString('λoperator.transform.regex $x /blah/ $a -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].op.pprint(), 'operator.transform.regex')
        self.assertEqual(result.clauses[0]._params[0].value, 'x')
        self.assertEqual(result.clauses[0]._params[1].value, re.compile('blah'))
        self.assertEqual(result.clauses[0]._params[2].value, 'a')
        self.assertIsNotNone(result.clauses[0]._rebind)

    def test_ternary_operator_rebind(self):
        result = TP.parseString('λoperator.transform.regex $x /blah/ $awef -> $q')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result.clauses), 1)
        self.assertEqual(result.clauses[0].op.pprint(), 'operator.transform.regex')
        self.assertEqual(result.clauses[0]._params[0].name, 'x')
        self.assertEqual(result.clauses[0]._params[1].name, re.compile('blah'))
        self.assertEqual(result.clauses[0]._params[2].name, 'awef')
        self.assertEqual(result.clauses[0]._rebind.name, 'q')

    def test_unary_format(self):
        result = TP.parseString('λoperator.transform.format $x blah -> $y')
        self.assertEqual(result.clauses[0].op.pprint(), 'operator.transform.format')


    # TODO test transform sugar, test multi variables, test sentences, test values
    @unittest.skip("TODO")
    def test_transform_sugar(self):
        return

if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.Trie_Transform_Parser_Tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
