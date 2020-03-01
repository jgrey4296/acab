#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import logging
from test_context import py_rule

class NumberTests(unittest.TestCase):

    @classmethod
    def setUpClas(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_number_parsing(self):
        pass


    def test_rule_with_transform(self):
            result = RP.parseString("a.rule:\n$x + 20 -> $y\n\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNone(result[0][1]._query)
            self.assertIsNotNone(result[0][1]._transform)


    def test_rule_with_multiple_transforms(self):
            result = RP.parseString("a.rule:\n $x + 20 -> $y, $y - 20\n\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNone(result[0][1]._query)
            self.assertIsNotNone(result[0][1]._transform)


    def test_rule_with_multiple_transforms_on_single_line(self):
            result = RP.parseString("a.rule:\n$x + 20 -> $y,$y - 20\n\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNone(result[0][1]._query)
            self.assertIsNotNone(result[0][1]._transform)


    def test_rule_with_query_transform_actions(self):
            result = RP.parseString("a.rule:\na.b.c?\n\n$x + 20\n\n+(a.b.c)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0][1], Rule)
            self.assertIsNotNone(result[0][1]._query)
            self.assertIsNotNone(result[0][1]._transform)
            self.assertEqual(len(result[0][1]._actions), 1)


    def test_rule_binding_expansion(self):
        bindings = { "x" : FP.parseString('a.b.c')[0],
                     "y" : FP.parseString('d.e.f')[0],
                     "z" : FP.parseString('x.y.z')[0] }
        result = RP.parseString("a.$x:\n$y.b.$z?\n\n$x + 2\n\n+($x)\nend")[0][1]
        expanded = result.expand_bindings(bindings)
        self.assertEqual(str(expanded),
                         "a.a.b.c:\n\td.e.f.b.x.y.z?\n\n\t$x + 2\n\n\t+(a.b.c)\nend")


    def test_basic_transform_core(self):
        result = TP.transform_core.parseString('$x + 20')[0]
        self.assertIsInstance(result, transform.OperatorTransform)
        self.assertIsInstance(result._op, transform.TransformOp)
        self.assertEqual(len(result._params), 2)


    def test_basic_transform_core_rebind(self):
        result = TP.transform_core.parseString('$y * 20 -> $z')[0]
        self.assertIsInstance(result, transform.OperatorTransform)
        self.assertIsInstance(result._op, transform.TransformOp)
        self.assertEqual(result._params[0]._value, "y")
        self.assertTrue(result._params[0]._data[KBU.BIND_S])
        self.assertEqual(result._params[1]._value, 20)
        self.assertIsNotNone(result._rebind)
        self.assertEqual(result._rebind._value, 'z')


    def test_basic_transform(self):
        result = TP.parseString('$x + 20, $y + 5')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._components), 2)


    def test_binary_operator(self):
        result = TP.parseString('$x + 20')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._components), 1)
        self.assertIsInstance(result._components[0]._op, transform.TransformOp)
        self.assertEqual(result._components[0]._params[0]._value, 'x')
        self.assertEqual(result._components[0]._params[1]._value, 20)
        self.assertIsNone(result._components[0]._rebind)


    def test_binary_rebind(self):
        result = TP.parseString('$x + 20 -> $y')
        self.assertIsInstance(result, transform.Transform)
        self.assertEqual(len(result._components), 1)
        self.assertIsInstance(result._components[0]._op, transform.TransformOp)
        self.assertEqual(result._components[0]._params[0]._value, 'x')
        self.assertEqual(result._components[0]._params[1]._value, 20)
        self.assertEqual(result._components[0]._rebind._value, 'y')


    def test_fact_str_equal(self):
        transforms = ["$x + 20", "$x + 20\n$y + 5",
                      "$xc - 10", "$x * 100",
                      "$x + 20 -> $y",
                      "$Blah + $bloo -> $BLEE",
                      "-$x", "_$x", "-$x -> $y",
                      "_$x -> $y", "$x ~= /blah/$a",
                      "$x ~= /aw+/$b -> $blah",
                      "$x + 2d5"
        ]
        parsed = [TP.parseString(x) for x in transforms]
        zipped = zip(transforms, parsed)
        for t,p in zipped:
            self.assertEqual(t,str(p))



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
