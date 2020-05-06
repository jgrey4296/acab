"""
"""
#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
from math import isclose
import pyparsing as pp
import unittest
import logging
import py_rule
from py_rule.abstract.contexts import Contexts
from py_rule.engines.trie_engine import TrieEngine
from py_rule.working_memory.trie_wm.parsing import TransformParser as TP
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.modules.values.numbers.number_module import NumberSpecification
from py_rule.modules.operators.standard_operators import StandardOperators


class TransformTests(unittest.TestCase):

    def setUp(self):
        ns = NumberSpecification()
        os = StandardOperators()
        self.e = TrieEngine(modules=[ns, os])

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_run_transform(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': 2, 'b': 4}, 'blah'))

        stub_transform = TP.parseString('$a AddOp 20 -> $y, $b MulOp 2 -> $z')

        # result = self.e._run_transform(stub_ctx[0], stub_transform)
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], 4)
        self.assertEqual(result['y'], 22)
        self.assertEqual(result['z'], 8)

    def test_run_transform_rebind(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': 2, 'b': 8}, "blah"))

        stub_transform = TP.parseString('$a AddOp 20 -> $q, $b MulOp $a -> $w')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], 8)
        self.assertEqual(result['q'], 22)
        self.assertEqual(result['w'], 16)

    def test_run_unary_transform(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': 2, 'b':-2, 'c': 2.53}, 'blah'))

        stub_transform = TP.parseString('NegOp $a -> $x, NegOp $b -> $y, RoundOp $c -> $z')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], -2)
        self.assertEqual(result['c'], 2.53)
        self.assertEqual(result['x'], -2)
        self.assertEqual(result['y'], 2)
        self.assertEqual(result['z'], 2)

    def test_run_unary_transform_rebind(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': 2, 'b':-2, 'c': 2.53}, 'blah'))

        stub_transform = TP.parseString('NegOp $a -> $x, NegOp $b -> $y, RoundOp $c -> $z')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], -2)
        self.assertEqual(result['c'], 2.53)
        self.assertEqual(result['x'], -2)
        self.assertEqual(result['y'], 2)
        self.assertEqual(result['z'], 2)

    def test_run_binary_transform(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a':2,'b':-2,'c':2.53},"blah"))

        stub_transform = TP.parseString('$a AddOp 20 -> $x, $b SubOp 20 -> $y, $c AddOp $x -> $z')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], -2)
        self.assertEqual(result['c'], 2.53)
        self.assertEqual(result['x'], 22)
        self.assertEqual(result['y'], -22)
        self.assertTrue(isclose(result['z'], 24.53))

    def test_run_binary_transform_rebind(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': 2, 'b':-2, 'c': 2.53}, 'blah'))

        stub_transform = TP.parseString('$a AddOp 20 -> $x, $b SubOp 20 -> $y, $c AddOp $a -> $z')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], -2)
        self.assertEqual(result['c'], 2.53)
        self.assertEqual(result['x'], 22)
        self.assertEqual(result['y'], -22)
        self.assertTrue(isclose(result['z'], 4.53))

    def test_run_ternary_regex_transform(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': "blah",
                          'b': "aaablah",
                          'c': "awefblahawef",
                          'd': 'AAAA'}, "blah"))

        stub_transform = TP.parseString('$a RegexOp /blah/ bloo -> $x, $b RegexOp /aaa\\w+/ $d -> $y, $c RegexOp /awef(\\w+)awef/ $d -> $z')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 'blah')
        self.assertEqual(result['b'], 'aaablah')
        self.assertEqual(result['c'], 'awefblahawef')
        self.assertEqual(result['x'], 'bloo')
        self.assertEqual(result['y'], 'AAAA')
        self.assertEqual(result['z'], 'AAAA')

    def test_run_ternary_regex_rebind(self):
        stub_ctx = Contexts()
        stub_ctx.append(({
            'a': "blah",
            'b':"aaablah",
            'c':"awefblahawef",
            'd':'AAAA'}, "blah"))

        stub_transform = TP.parseString('$a RegexOp /blah/ bloo -> $x, $b RegexOp /aaa\\w+/ $d -> $y, $c RegexOp /awef(\\w+)awef/ $d -> $z')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 'blah')
        self.assertEqual(result['b'], 'aaablah')
        self.assertEqual(result['c'], 'awefblahawef')
        self.assertEqual(result['x'], 'bloo')
        self.assertEqual(result['y'], 'AAAA')
        self.assertEqual(result['z'], 'AAAA')

    def test_run_unary_format(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': "AAA",
                          'b': "BBB",
                          'c': "CCC",
                          'x': "{a}",
                          'y' : "{a} blah {b}",
                          'z': "{c} {b} {a}"}, "blah"))

        stub_transform = TP.parseString('FormatOp $x -> $q, FormatOp $y -> $w, FormatOp $z -> $e')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['x'], '{a}')
        self.assertEqual(result['y'], '{a} blah {b}')
        self.assertEqual(result['z'], '{c} {b} {a}')
        self.assertEqual(result['q'], 'AAA')
        self.assertEqual(result['w'], 'AAA blah BBB')
        self.assertEqual(result['e'], 'CCC BBB AAA')

    def test_run_unary_format_rebind(self):
        stub_ctx = Contexts()
        stub_ctx.append(({'a': "AAA",
                          'b': "BBB",
                          'c': "CCC",
                          'x': "{a}",
                          'y': "{a} blah {b}",
                          'z': "{c} {b} {a}"}, "blah"))

        stub_transform = TP.parseString('FormatOp $x -> $xa, FormatOp $y -> $ya, FormatOp $z -> $za')
        result = stub_transform(stub_ctx[0], None)[0]
        self.assertIsInstance(result, dict)
        self.assertEqual(result['xa'], 'AAA')
        self.assertEqual(result['ya'], 'AAA blah BBB')
        self.assertEqual(result['za'], 'CCC BBB AAA')



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
