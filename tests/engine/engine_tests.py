import unittest
import logging
from test_context import py_rule
from py_rule.abstract.contexts import Contexts
import py_rule.engines.trie_engine as T
import py_rule.knowledge_bases.trie_kb.parsing.TransformParser as TP
import py_rule.knowledge_bases.trie_kb.parsing.ActionParser as AP
from math import isclose

class Engine_Tests(unittest.TestCase):

    def setUp(self):
        self.e = T.TrieEngine()


    def tearDown(self):
        self.e = None

    #----------
    #use testcase snippets
    def test_init(self):
        self.assertIsNotNone(self.e)

    def test_assert(self):
        self.assertEqual(len(self.e._knowledge_base._internal_trie._root), 0)
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._knowledge_base._internal_trie._root), 1)

    def test_retract(self):
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._knowledge_base._internal_trie._root), 1)
        self.e.retract('a')
        self.assertEqual(len(self.e._knowledge_base._internal_trie._root), 0)

    def test_query(self):
        self.e.add('a.b.c')
        result = self.e.query('a.b.c?')
        self.assertTrue(bool(result))

    def test_query_fail(self):
        self.assertFalse(self.e.query('a.b.c?'))

    def test_query_fail_with_asserted_facts(self):
        self.e.add('a.b.c, a.b.d')
        result = self.e.query('a.b.c?, a.b.e?')
        self.assertFalse(self.e.query('a.b.c?, a.b.e?'))

    def test_query_with_binds(self):
        self.e.add('a.b.c, a.b.d, a.d.c')
        self.assertTrue(self.e.query('a.b.$x?, a.d.$x?'))

    def test_query_with_binds_fail(self):
        self.e.add('a.b.c, a.b.d, a.d.e')
        self.assertFalse(self.e.query('a.b.$x?, a.d.$x?'))

    def test_multi_assert(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        self.assertEqual(len(self.e._knowledge_base._internal_trie.get_nodes(pred=lambda x: not bool(x))), 3)
        self.assertTrue(self.e.query('a.b.c?, a.b.d?, a.b.e?'))

    def test_multi_retract(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        self.assertEqual(len(self.e._knowledge_base._internal_trie.get_nodes(pred=lambda x: not bool(x))), 3)
        self.e.retract('a.b.e, a.b.d')
        self.assertEqual(len(self.e._knowledge_base._internal_trie.get_nodes(pred=lambda x: not bool(x))), 1)

    def test_multi_clause_query(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        result = self.e.query('a.b.c?, a.b.d?, a.b.e?')
        self.assertTrue(result)

    @unittest.skip("Broken")
    def test_rule_registration(self):
        self.assertEqual(len(self.e._rules), 0)
        self.e.registerRules("a.test.rule:\nend")
        self.assertEqual(len(self.e._rules), 1)
        self.e.registerRules("a.second.rule:\nend")
        self.assertEqual(len(self.e._rules), 2)

    @unittest.skip("Broken")
    def test_rule_registration_overwrite(self):
        self.assertEqual(len(self.e._rules), 0)
        self.e.registerRules("a.test.rule:\nend")
        self.assertEqual(len(self.e._rules), 1)
        self.e.registerRules("a.test.rule:\nend")
        self.assertEqual(len(self.e._rules), 1)


    def _test_register_action(self):
        self.assertEqual(len(self.e._custom_actions), 0)
        self.e.register_action("Test_Func", lambda e, p: logging.info("called"))
        self.assertEqual(len(self.e._custom_actions), 1)
        self.assertTrue(True)

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


    @unittest.skip("Broken")
    def test_run_assert_action(self):
        actions = AP.parseString("+(a.b.c)")
        self.assertFalse(self.e.query("a.b.c?"))
        self.e._run_actions({},actions)
        self.assertTrue(self.e.query("a.b.c?"))

    @unittest.skip("Broken")
    def test_run_retract_action(self):
        actions = AP.parseString("-(a.b.c)")
        self.e.add("a.b.c")
        self.assertTrue(self.e.query("a.b.c?"))
        self.e._run_actions({}, actions)
        self.assertFalse(self.e.query("a.b.c?"))
        self.assertTrue(self.e.query("~a.b.c?"))

    @unittest.skip("Broken")
    def test_run_assert_multi_action(self):
        actions = AP.parseString("+(a.b.c), +(a.b.d)")
        self.assertFalse(self.e.query("a.b.c?, a.b.d?"))
        self.assertTrue(self.e.query("~a.b.c?, ~a.b.d?"))
        self.e._run_actions({}, actions)
        self.assertTrue(self.e.query("a.b.c?, a.b.d?"))

    @unittest.skip("Broken")
    def test_run_mixed_multi_action(self):
        actions = AP.parseString("+(a.b.c), -(a.b.d)")
        self.e.add("a.b.d")
        self.assertTrue(self.e.query("~a.b.c?, a.b.d?"))
        self.e._run_actions({}, actions)
        self.assertTrue(self.e.query("a.b.c?, ~a.b.d?"))

    @unittest.skip("Broken")
    def test_run_bound_assert_action(self):
        data = {"x": "blah"}
        actions = AP.parseString("+(a.b.$x)")
        self.assertTrue(self.e.query("~a.b.blah?"))
        self.e._run_actions(data, actions)
        self.assertTrue(self.e.query("a.b.blah?"))

    @unittest.skip("Broken")
    def test_run_bound_retract_action(self):
        data = {"blah" : "bloo"}
        actions = AP.parseString("-(a.$blah.c)")
        self.e.add("a.bloo.c")
        self.assertTrue(self.e.query("a.bloo.c?"))
        self.e._run_actions(data, actions)
        self.assertTrue(self.e.query("~a.bloo.c?, a.bloo?"))

    @unittest.skip("Broken")
    def test_run_mixed_bound_actions(self):
        data = {"blah": "bloo"}
        actions = AP.parseString("+(a.$blah), -(b.$blah)")
        self.e.add("b.bloo")
        self.assertTrue(self.e.query("b.bloo?"))
        self.e._run_actions(data, actions)
        self.assertTrue(self.e.query("a.bloo?, ~b.bloo?"))

    @unittest.skip("Broken")
    def test_register_and_run_entire_rule(self):
        self.e.registerRules("test.rule:\na.$x?\n\n$x + 20 -> $y\n\n+(b.$y)\nend")
        self.e.add("a.20")
        self.assertTrue(self.e.query("a.20?, ~b.40?"))
        self.e._run_rules()
        self.assertTrue(self.e.query("a.20?, b.40?"))

    @unittest.skip("Broken")
    def test_register_rule_added_to_trie(self):
        self.assertTrue(self.e.query('~test.rule?'))
        self.e.registerRules('test.rule:\na.b.c?\n\n+(a.b.d)\nend')
        self.assertTrue(self.e.query('test.rule?'))

    # def test_rule_query(self):
    #     self.e.add('.a.b.c')
    #     self.e.registerRules('.test.rule:\n.a.b.c?\n\n+(.d.e.f)\nend')
    #     self.assertTrue(self.e.query('.test.rule(^rule)?'))
    #     self.assertFalse(self.e.query('.a.b.c(^rule)?'))
    #     self.assertTrue(self.e.query('~.a.b.c(^rule)?'))


if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.engine_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
