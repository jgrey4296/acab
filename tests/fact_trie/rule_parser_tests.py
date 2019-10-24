import unittest
import logging
import IPython
from test_context import py_rule
from py_rule.fact_trie.parsing import RuleParser as RP
from py_rule.fact_trie.parsing import FactParser as FP
from py_rule.abstract.rule import Rule
from py_rule.abstract.query import Query

class Trie_Rule_Parser_Tests(unittest.TestCase):

    def setUp(self):
            return 1

    def tearDown(self):
            return 1

    #----------
    #use testcase snippets
    def test_init(self):
            self.assertIsNotNone(RP)

    def test_name_empty_rule_parse(self):
            result = RP.parseString("a.rule:\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertEqual(str(result[0]._name), "a.rule")

    def test_multi_empty_rules(self):
            result = RP.parseString("a.rule:\nend\n\na.second.rule:\nend")
            self.assertEqual(len(result),2)
            self.assertTrue(all([isinstance(x,Rule) for x in result]))

    def test_rule_with_query(self):
            result = RP.parseString("a.rule:\na.b.c?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNotNone(result[0]._query)
            self.assertIsInstance(result[0]._query, Query)

    def test_rule_with_multi_clause_query(self):
            result = RP.parseString("a.rule:\na.b.c?,\na.b.d?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNotNone(result[0]._query)
            self.assertIsInstance(result[0]._query, Query)
            self.assertEqual(len(result[0]._query), 2)

    def test_rule_with_multi_clauses_in_one_line(self):
            result = RP.parseString("a.rule:\na.b.c?, a.b.d?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNotNone(result[0]._query)
            self.assertIsInstance(result[0]._query, Query)
            self.assertEqual(len(result[0]._query), 2)

    def test_rule_with_binding_query(self):
            result = RP.parseString("a.rule:\na.b.$x?\n\nend")
            self.assertEqual(len(result),1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNotNone(result[0]._query)
            self.assertIsInstance(result[0]._query, Query)
            self.assertEqual(len(result[0]._query), 1)

    def test_rule_with_transform(self):
            result = RP.parseString("a.rule:\n$x + 20 -> $y\n\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNone(result[0]._query)
            self.assertIsNotNone(result[0]._transform)

    def test_rule_with_multiple_transforms(self):
            result = RP.parseString("a.rule:\n $x + 20 -> $y, $y - 20\n\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNone(result[0]._query)
            self.assertIsNotNone(result[0]._transform)

    def test_rule_with_multiple_transforms_on_single_line(self):
            result = RP.parseString("a.rule:\n$x + 20 -> $y,$y - 20\n\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNone(result[0]._query)
            self.assertIsNotNone(result[0]._transform)


    def test_rule_with_actions(self):
            result = RP.parseString("a.rule:\n+(a.b.c)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNone(result[0]._query)
            self.assertIsNone(result[0]._transform)
            self.assertEqual(len(result[0]._actions), 1)

    def test_multi_action_rule(self):
            result = RP.parseString("a.rule:\n+(a.b.c),\n-(a.b.d)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNone(result[0]._query)
            self.assertIsNone(result[0]._transform)
            self.assertEqual(len(result[0]._actions), 2)

    def test_multi_action_single_line_rule(self):
            result = RP.parseString("a.rule:\n+(a.b.c), -(a.b.d)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNone(result[0]._query)
            self.assertIsNone(result[0]._transform)
            self.assertEqual(len(result[0]._actions), 2)

    def test_rule_with_query_transform_actions(self):
            result = RP.parseString("a.rule:\na.b.c?\n\n$x + 20\n\n+(a.b.c)\nend")
            self.assertEqual(len(result), 1)
            self.assertIsInstance(result[0], Rule)
            self.assertIsNotNone(result[0]._query)
            self.assertIsNotNone(result[0]._transform)
            self.assertEqual(len(result[0]._actions), 1)

    def test_rule_simple_binding_expansion(self):
        bindings = { "x" : FP.parseString('a.b.c')[0] }
        result = RP.parseString("a.rule:\n$x?\n\nend")[0]
        expanded = result.expand_bindings(bindings)
        self.assertEqual(str(expanded),
                         "a.rule:\n\ta.b.c?\n\nend")

    def test_rule_binding_expansion(self):
        bindings = { "x" : FP.parseString('a.b.c')[0],
                     "y" : FP.parseString('d.e.f')[0],
                     "z" : FP.parseString('x.y.z')[0] }
        result = RP.parseString("a.$x:\n$y.b.$z?\n\n$x + 2\n\n+($x)\nend")[0]
        expanded = result.expand_bindings(bindings)
        self.assertEqual(str(expanded),
                         "a.a.b.c:\n\td.e.f.b.x.y.z?\n\n\t$x + 2\n\n\t+(a.b.c)\nend")

    def test_rule_tags(self):
            result = RP.parseString('a.test.rule:\n#blah, #bloo, #blee\n\na.b.c?\n\n+(a.b.c)\nend')[0]
            self.assertIsInstance(result, Rule)
            self.assertEqual(str(result._name), "a.test.rule")
            self.assertTrue(all(x in result._tags for x in ["blah","bloo","blee"]))

    def test_fact_str_equal(self):
            rules = [ "a.rule:\nend",
                      "a.rule:\n\ta.b.c?\n\nend",
                      "a.rule:\n\ta.b.c?\n\ta.b!d?\n\nend",
                      "a.different.rule:\n\ta.b.c?\n\n\t$x + 20\n\nend",
                      "a.rule:\n\ta.b.c?\n\n\t$x + 20 -> $y\n\nend",
                      "a.rule:\n\ta.b.c?\n\n\t$x * 10 -> $AB\n\n\t+(a.b.d)\nend",
                      "a.rule:\n\t#blah, #blee, #bloo\n\nend"]

            parsed = [RP.parseString(x)[0] for x in rules]
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
end"""
        result = RP.parseString(rulestr)[0]
        self.assertIsInstance(result, Rule)


if __name__ == "__main__":
      LOGLEVEL = logging.INFO
      logFileName = "log.Trie_Rule_Parser_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
