from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.nodes.trie_node import TrieNode
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.modules.analysis.typing import util as TU
from py_rule.modules.analysis.typing.type_definition import TypeDefinition
import logging
import py_rule.modules.analysis.typing.parsing.TypeDefParser as TD
import py_rule.modules.analysis.typing.parsing.TypeParser as TP
import py_rule.util as util
import random
import unittest


class Trie_Fact_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        FP.HOTLOAD_ANNOTATIONS << TP.TYPEDEC_CORE
        TP.HOTLOAD_BASIC_SEN << FP.BASIC_SEN
        TD.HOTLOAD_VALBIND << FP.VALBIND
        TD.HOTLOAD_BASIC_SEN << FP.BASIC_SEN
        TD.HOTLOAD_PARAM_SEN << FP.PARAM_SEN

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    #use testcase snippets
    def test_trivial(self):
        self.assertIsNotNone(TD.parseString)
        self.assertIsNotNone(TD.TYPEDEF)
        self.assertIsNotNone(TD.OP_DEF)

    def test_basic_typedef(self):
        result = TD.parseString('σ::blah:\n\na.b.c\n\nend')[0]
        self.assertIsInstance(result[1], TypeDefinition)
        self.assertEqual(len(result[1]._vars), 0)
        self.assertEqual(result[1]._name[0]._value, "blah")
        self.assertEqual(len(result[1]._structure), 1)

    def test_typedef_with_variable(self):
        result = TD.parseString('::blah($x):\n\na.b.c\n\nend')[0]
        self.assertIsInstance(result[1], TypeDefinition)
        self.assertEqual(len(result[1]._vars), 1)
        self.assertEqual(result[1]._vars[0]._value, "x")

    def test_typedef_with_multi_variables(self):
        result = TD.parseString('::blah($x, $y):\n\na.b.c\n\nend')[0]
        self.assertIsInstance(result[1], TypeDefinition)
        self.assertEqual(len(result[1]._vars), 2)
        var_set = set([x._value for x in result[1]._vars])
        match_set = set(["x", "y"])
        self.assertEqual(var_set, match_set)

    def test_typedef_with_structure_types(self):
        result = TD.parseString('::blah:\n\na.b.c(::bloo)\n\nend')[0]
        self.assertEqual(result[1]._structure[0][-1]._data[TU.TYPE_DEC_S]._name._value, 'bloo')

    def test_typedef_with_bad_vars(self):
        # TODO: replace exception with a more specific one
        with self.assertRaises(Exception):
            result = TD.parseString('::blah(blee):\n\na.b.c\n\nend')[0]



    def test_op_def_parse(self):
        the_string = 'λ::AddOp: $x(::Num).$x.$x => +'

        result = TD.OP_DEF.parseString(the_string)



if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.trie_typedef_parser"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
