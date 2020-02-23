"""
TODO Implement transform tests
TODO implement transform rebind
"""
#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from test_context import py_rule


class TransformTests(unittest.TestCase):

    @classmethod
    def setUpClas(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    @unittest.skip("Broken")
    def test_run_transform(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = 2
        stub_ctx[0]['b'] = 4

        stub_transform = TP.parseString('$a + 20, $b * 2')

        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 22)
        self.assertEqual(result['b'], 8)

    @unittest.skip("Broken")
    def test_run_transform_rebind(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = 2
        stub_ctx[0]['b'] = 8

        stub_transform = TP.parseString('$a + 20 -> $q, $b * $a -> $w')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], 8)
        self.assertEqual(result['q'], 22)
        self.assertEqual(result['w'], 16)

    @unittest.skip("Broken")
    def test_run_unary_transform(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = 2
        stub_ctx[0]['b'] = -2
        stub_ctx[0]['c'] = 2.53

        stub_transform = TP.parseString('-$a, -$b, _$c')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], -2)
        self.assertEqual(result['b'], 2)
        self.assertEqual(result['c'], 2)

    @unittest.skip("Broken")
    def test_run_unary_transform_rebind(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = 2
        stub_ctx[0]['b'] = -2
        stub_ctx[0]['c'] = 2.53

        stub_transform = TP.parseString('-$a -> $x, -$b -> $y, _$c -> $z')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], -2)
        self.assertEqual(result['c'], 2.53)
        self.assertEqual(result['x'], -2)
        self.assertEqual(result['y'], 2)
        self.assertEqual(result['z'], 2)

    @unittest.skip("Broken")
    def test_run_binary_transform(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = 2
        stub_ctx[0]['b'] = -2
        stub_ctx[0]['c'] = 2.53

        stub_transform = TP.parseString('$a + 20, $b - 20, $c + $a')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 22)
        self.assertEqual(result['b'], -22)
        self.assertEqual(result['c'], 24.53)

    @unittest.skip("Broken")
    def test_run_binary_transform_rebind(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = 2
        stub_ctx[0]['b'] = -2
        stub_ctx[0]['c'] = 2.53

        stub_transform = TP.parseString('$a + 20 -> $x, $b - 20 -> $y, $c + $a -> $z')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 2)
        self.assertEqual(result['b'], -2)
        self.assertEqual(result['c'], 2.53)
        self.assertEqual(result['x'], 22)
        self.assertEqual(result['y'], -22)
        self.assertTrue(isclose(result['z'], 4.53))

    @unittest.skip("Broken")
    def test_run_ternary_regex_transform(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = "blah"
        stub_ctx[0]['b'] = "aaablah"
        stub_ctx[0]['c'] = "awefblahawef"
        stub_ctx[0]['d'] = 'AAAA'

        stub_transform = TP.parseString('$a ~= /blah/ bloo, $b ~= /aaa\\w+/ $d, $c ~= /awef(\\w+)awef/ $d')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 'bloo')
        self.assertEqual(result['b'], 'AAAA')
        self.assertEqual(result['c'], 'AAAA')

    @unittest.skip("Broken")
    def test_run_ternary_regex_rebind(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = "blah"
        stub_ctx[0]['b'] = "aaablah"
        stub_ctx[0]['c'] = "awefblahawef"
        stub_ctx[0]['d'] = 'AAAA'

        stub_transform = TP.parseString('$a ~= /blah/ bloo -> $x, $b ~= /aaa\\w+/ $d -> $y, $c ~= /awef(\\w+)awef/ $d -> $z')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['a'], 'blah')
        self.assertEqual(result['b'], 'aaablah')
        self.assertEqual(result['c'], 'awefblahawef')
        self.assertEqual(result['x'], 'bloo')
        self.assertEqual(result['y'], 'AAAA')
        self.assertEqual(result['z'], 'AAAA')

    @unittest.skip("Broken")
    def test_run_unary_format(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = "AAA"
        stub_ctx[0]['b'] = "BBB"
        stub_ctx[0]['c'] = "CCC"
        stub_ctx[0]['x'] = "{a}"
        stub_ctx[0]['y'] = "{a} blah {b}"
        stub_ctx[0]['z'] = "{c} {b} {a}"

        stub_transform = TP.parseString('~{} $x, ~{} $y, ~{} $z')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
        self.assertIsInstance(result, dict)
        self.assertEqual(result['x'], 'AAA')
        self.assertEqual(result['y'], 'AAA blah BBB')
        self.assertEqual(result['z'], 'CCC BBB AAA')

    @unittest.skip("Broken")
    def test_run_unary_format_rebind(self):
        stub_ctx = Contexts.initial(None)
        stub_ctx[0]['a'] = "AAA"
        stub_ctx[0]['b'] = "BBB"
        stub_ctx[0]['c'] = "CCC"
        stub_ctx[0]['x'] = "{a}"
        stub_ctx[0]['y'] = "{a} blah {b}"
        stub_ctx[0]['z'] = "{c} {b} {a}"

        stub_transform = TP.parseString('~{} $x -> $xa, ~{} $y -> $ya, ~{} $z -> $za')
        result = self.e._run_transform(stub_ctx[0], stub_transform)
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
