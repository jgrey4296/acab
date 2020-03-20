import unittest
import logging
import random
import pyparsing as pp
import py_rule.working_memory.trie_wm.parsing.FactParser as FP
from py_rule.abstract.trie.nodes.trie_node import TrieNode
from py_rule.abstract.sentence import Sentence
from py_rule.working_memory.trie_wm import util as KBU


class Trie_Fact_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_trivial(self):
        self.assertIsNotNone(FP.parseString)
        self.assertIsNotNone(FP.PARAM_SEN)
        self.assertIsNotNone(FP.PARAM_SEN_PLURAL)

    def test_parseString(self):
        result = FP.parseString('a.b.c')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(all([isinstance(x, TrieNode) for x in result]))
        self.assertEqual(str(result), "a.b.c")
        self.assertTrue(all([x._data[KBU.OPERATOR_S] == KBU.EXOP.DOT for x in result]))

    def test_parseStrings(self):
        result = FP.parseString('a.b.c,\n b.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence) for x in result]))

    def test_param_fact_string(self):
        result = FP.PARAM_SEN.parseString('a.b.$x')[0]
        self.assertIsNotNone(result)
        self.assertTrue(result[-1]._data[KBU.BIND_S])

    def test_exclusion_operator_parsing(self):
        result = FP.parseString('a!b!c')[0]
        self.assertTrue(all([x._data[KBU.OPERATOR_S] == KBU.EXOP.EX for x in result[:-1]]))

    def test_strings(self):
        result = FP.parseString('a.b."This is a test"!c')[0]
        self.assertEqual(len(result), 4)
        self.assertEqual(result[2]._value, "This is a test")

    def test_bind_addition_to_node_recognition(self):
        result = FP.parseString('$a.$b!$c')[0]
        for x in result:
            self.assertTrue(x._data[KBU.BIND_S])

    def test_fact_leading_bind(self):
        result = FP.parseString('$x.a.b.c')[0]
        self.assertTrue(result[0]._data[KBU.BIND_S])

    def test_fact_str_equal(self):
        actions = ["a.b.c",
                   "a.b!c",
                   'a.b."a string".c',
                   'a.b!"a string"!c',
                   'a.b.$x',
                   'a!$x!y']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for a,p in zipped:
            self.assertEqual(a,str(p))

    def test_leading_bind_str_equal(self):
        actions = ['$x.a.b.c', '$y!b.c', '$x.$y!$z']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for a,p in zipped:
            self.assertEqual(a, str(p))

    def test_binding_expansion(self):
        bindings = { "a" : FP.parseString("blah")[0],
                     "b": FP.parseString("bloo")[0] }
        result = FP.parseString('$a.b.$b!c')[0]
        expanded = result.expand_bindings(bindings)
        asString = str(expanded)
        self.assertEqual(asString, "blah.b.bloo!c")

    def test_valbind_expansion(self):
        """ Test added new parsers to the valbind parser """
        new_parser = pp.Word("¿awef")
        new_parser.setParseAction(lambda t: ("awef", t[0]))

        FP.HOTLOAD_VALUES << new_parser

        a = FP.VALBIND.parseString("¿awef")[0]
        self.assertEqual(a._value, "¿awef")
        self.assertEqual(a._data[KBU.VALUE_TYPE_S], "awef")


if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.trie_fact_parser_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
