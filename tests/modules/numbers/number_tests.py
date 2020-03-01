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


    def test_simple_action_parse(self):
        result = AP.parseString("+(20, 30, 40)")[0]
        self.assertIsInstance(result, action.Action)
        self.assertEqual(result._op._op_str, '+')
        self.assertEqual([x[-1]._value for x in result._values], [20, 30, 40])


    def test_custom_action_parse(self):
        result = AP.parseString("blah(20, 30, 40)")
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0], action.Action)
        self.assertEqual(result[0]._op._op_str, "blah")
        self.assertEqual([x[-1]._value for x in result[0]._values], [20, 30, 40])


    def test_actions_parse(self):
        result = AP.parseString('+(2), -(3), @(4)')
        self.assertEqual(len(result), 3)
        self.assertTrue(all([isinstance(x, action.Action) for x in result]))
        for parsed_action, op in zip(result, ["+", "-", "@"]):
            self.assertEqual(parsed_action._op._op_str, op)


    def test_action_str_equal(self):
        actions = ["+(2)", "-(3)", "@(4)"]
        parsed = [AP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        self.assertTrue(all([x == str(y) for x,y in zipped]))


    def test_numbers_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = round(random.random() * 1000)
            result = FP.parseString('a.' + str(r))[0]
            self.assertEqual(result[-1]._value, r)


    def test_negative_number_parsing(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            r = - round(random.random() * mult)
            result = FP.parseString('a.'+str(r))[0]
            self.assertEqual(result[-1]._value, r)


    def test_floats(self):
        for i in range(100):
            mult = 10 ** round(random.random() * 4)
            a = round(random.random() * mult)
            b = round(random.random() * mult)
            float_form = float(str(a) + "." + str(b))
            d_form = str(a) + "d" + str(b)
            result = FP.parseString('a.'+d_form)[0]
            self.assertEqual(result[-1]._value, float_form)


    def test_basic_comp_internal(self):
        result = QP.COMP_Internal.parseString('>20')[0]
        self.assertIsInstance(result, Comparison)


    def test_basic_comparison(self):
        result = QP.constraints.parseString('>20, <40, !=$x, ==$y, ~=/blah/')[0]
        self.assertEqual(result[0], KBU.CONSTRAINT_S)
        self.assertEqual(len(result[1]), 5)
        self.assertTrue(all([isinstance(x, Comparison) for x in result[1]]))
        self.assertIsInstance(result[1][0]._op, type(CompOp.op_list['>']))
        self.assertIsInstance(result[1][1]._op, type(CompOp.op_list['<']))
        self.assertIsInstance(result[1][2]._op, type(CompOp.op_list['!=']))
        self.assertIsInstance(result[1][3]._op, type(CompOp.op_list['==']))
        self.assertIsInstance(result[1][4]._op, type(CompOp.op_list['~=']))


    def test_basic_query_core(self):
        result = QP.QueryCore.parseString('a(>20).')[0]
        self.assertTrue(KBU.CONSTRAINT_S in result._data)
        self.assertEqual(len(result._data[KBU.CONSTRAINT_S]), 1)
        self.assertIsInstance(result._data[KBU.CONSTRAINT_S][0], Comparison)


    def test_basic_query_core_multi_comparison(self):
        result = QP.QueryCore.parseString('a(>20, <30).')[0]
        self.assertEqual(len(result._data[KBU.CONSTRAINT_S]), 2)
        self.assertTrue(all([isinstance(x, Comparison) for x in result._data[KBU.CONSTRAINT_S]]))


    def test_basic_query_core_with_exclusion(self):
        result = QP.QueryCore.parseString('a(>20)!')[0]
        self.assertEqual(result._data[KBU.OPERATOR_S], KBU.EXOP.EX)


    def test_clause_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result._fallback)
        self.assertEqual(len(result._fallback), 1)

        self.assertEqual(result._fallback[0][0], 'x')
        self.assertEqual(result._fallback[0][1][-1]._value, 2)


    def test_clause_negated_fallback(self):
        with self.assertRaises(Exception):
            QP.clause.parseString('~a.b.c? || $x:2')


    def test_clause_multi_fallback(self):
        result = QP.clause.parseString('a.b.c? || $x:2, $y:5')[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsNotNone(result._fallback)
        self.assertEqual(len(result._fallback), 2)
        self.assertEqual(result._fallback[0][0], 'x')
        self.assertEqual(result._fallback[0][1][-1]._value, 2)
        self.assertEqual(result._fallback[1][0], 'y')
        self.assertEqual(result._fallback[1][1][-1]._value, 5)


    def test_fact_str_equal(self):
        queries = ["a.b.c?", "a.b!c?", 'a.b."a string".c?',
                   'a.b!"a string"!c?', 'a.b(> 20)?',
                   'a.$b?', 'a!$b?', 'a.$b(> $c)?',
                   'a.$b(> 20, < 40, != $x, == $y)?',
                   '~a.b.c?', '~a!b.c?',
                   'a.$b(~= /blah/)?',
                   'a.b.c? || $x:2',
                   'a.b.d? || $x:5, $y:blah']
                   # 'a.b.c(^$x)?']
        parsed = [QP.parseString(x) for x in queries]
        zipped = zip(queries, parsed)
        for a,q in zipped:
            self.assertEqual(a,str(q))


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


    def test_fact_str_equal(self):
            rules = [ "a.rule:\nend",
                      "a.rule:\n\ta.b.c?\n\nend",
                      "a.rule:\n\ta.b.c?\n\ta.b!d?\n\nend",
                      "a.different.rule:\n\ta.b.c?\n\n\t$x + 20\n\nend",
                      "a.rule:\n\ta.b.c?\n\n\t$x + 20 -> $y\n\nend",
                      "a.rule:\n\ta.b.c?\n\n\t$x * 10 -> $AB\n\n\t+(a.b.d)\nend",
                      "a.rule:\n\t#blah, #blee, #bloo\n\nend"]

            parsed = [RP.parseString(x)[0][1] for x in rules]
            zipped = zip(rules, parsed)
            for r,p in zipped:
                  self.assertEqual(r,str(p))


    def test_bdi_rule_parse(self):
        rulestr = """bdi.blah:
    #propose
    count!$x(< 10)?

    $x + 2 -> $y
    ~{} "Hello: {x}" -> $z

    @($z)
    +(count!$y)
end
        """
        result = RP.parseString(rulestr)[0]
        self.assertEqual(result[0], KBU.RULE_S)
        self.assertIsInstance(result[1], Rule)


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


    def test_query_alpha_comp(self):
        """ Check that alpha comparisons work """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(==20)?')
        self.assertTrue(result)


    def test_query_alpha_comp_fails(self):
        """ Check that alpha comparisons can fail """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(==30)?')
        self.assertFalse(result)


    def test_query_alpha_comp_GT(self):
        """ Check that other comparisons from equality can be tested for """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(>10)?')
        self.assertTrue(result)


    def test_query_fail(self):
        """ Check that other comparisons can fail """
        self.trie.add('a.b.20')
        result = self.trie.query('a.b.$x(>30)?')
        self.assertFalse(result)


    def test_query_multi_bind_comp(self):
        """ Check that bindings hold across clauses """
        self.trie.add('a.b.20, a.c.30, a.d.40')
        result = self.trie.query('a.c.$x?, a.$y(!=c).$v(>$x)?')
        self.assertTrue(result)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['x'], 30)
        self.assertEqual(result[0]['y'], 'd')
        self.assertEqual(result[0]['v'], 40)


    def test_query_multi_alts(self):
        """ Check that queries with bindings provide enumerated alternatives """
        self.trie.add('a.b.20, a.c.30, a.d.40, a.e.50')
        result = self.trie.query('a.c.$x?, a.$y(!=c).$v(>$x)?')
        self.assertTrue(result)
        self.assertEqual(len(result), 2)






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
