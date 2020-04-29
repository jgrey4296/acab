import unittest
import logging
from py_rule.abstract.contexts import Contexts
import py_rule.engines.trie_engine as T
import py_rule.working_memory.trie_wm.parsing.TransformParser as TP
import py_rule.working_memory.trie_wm.parsing.ActionParser as AP
from py_rule.abstract.rule import Rule
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
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 0)
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 1)

    def test_retract(self):
        self.e.add('a.b.c')
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 1)
        self.e.add('~a')
        self.assertEqual(len(self.e._working_memory._internal_trie._root), 0)

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
        self.assertEqual(len(self.e._working_memory._internal_trie.get_nodes(pred=lambda x: not bool(x))), 3)
        self.assertTrue(self.e.query('a.b.c?, a.b.d?, a.b.e?'))

    def test_multi_retract(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        self.assertEqual(len(self.e._working_memory._internal_trie.get_nodes(pred=lambda x: not bool(x))), 3)
        self.e.add('~a.b.e, ~a.b.d')
        self.assertEqual(len(self.e._working_memory._internal_trie.get_nodes(pred=lambda x: not bool(x))), 1)

    def test_multi_clause_query(self):
        self.e.add('a.b.c, a.b.d, a.b.e')
        result = self.e.query('a.b.c?, a.b.d?, a.b.e?')
        self.assertTrue(result)

    def test_rule_assertion(self):
        self.assertFalse(self.e.query('a.test.rule?'))
        self.e.add('a.test.rule: (::ρ)\na.b.c?\n\na.b.d\nend')
        results = self.e.query('a.test.$rule?')
        self.assertTrue(results)
        self.assertIsInstance(results[0]['rule'], Rule)


    # TODO: in place of action registration, check operators are called appropriately

if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.engine_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
