import unittest
import logging
import random
from test_context import py_rule
import py_rule.fact_trie.parsing.TypeDefParser as TD
from py_rule.trie.nodes.trie_node import TrieNode
from py_rule.abstract.sentence import Sentence
from py_rule.typing.ex_types import TypeDefinition
import py_rule.utils as util
import IPython

class Trie_Fact_Parser_Tests(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_trivial(self):
        self.assertIsNotNone(TD.parseString)
        self.assertIsNotNone(TD.TYPEDEF)

    def test_basic_typedef(self):
        result = TD.parseString('::blah:\n\na.b.c\n\nEND')[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result._vars), 0)
        self.assertEqual(result._name._value, "blah")
        self.assertEqual(len(result._structure), 1)

    def test_typedef_with_variable(self):
        result = TD.parseString('::blah($x):\n\na.b.c\n\nEND')[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result._vars), 1)
        self.assertEqual(result._vars[0]._value, "x")

    def test_typedef_with_multi_variables(self):
        result = TD.parseString('::blah($x, $y):\n\na.b.c\n\nEND')[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result._vars), 2)
        var_set = set([x._value for x in result._vars])
        match_set = set(["x", "y"])
        self.assertEqual(var_set, match_set)

    def test_typedef_with_structure_types(self):
        result = TD.parseString('::blah:\n\na.b.c(::bloo)\n\nEND')[0]
        self.assertEqual(result._structure[0][-1]._data['typedec']._name._value, 'bloo')

    def test_typedef_with_bad_vars(self):
        with self.assertRaises(Exception):
            result = TD.parseString('::blah(blee):\n\na.b.c\n\nEND')[0]


if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.trie_typedef_parser"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
