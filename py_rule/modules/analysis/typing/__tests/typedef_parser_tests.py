import logging
import random
import unittest

import py_rule.modules.analysis.typing.parsing.TypeDefParser as TD
import py_rule.modules.analysis.typing.parsing.TypeParser as TP
import py_rule.util as util

from py_rule.abstract.sentence import Sentence
from py_rule.abstract.node import PyRuleNode
from py_rule.error.pyrule_parse_exception import PyRuleParseException
from py_rule.modules.analysis.typing import util as TU
from py_rule.modules.analysis.typing.values.type_definition import TypeDefinition
from py_rule.modules.analysis.typing.values.operator_definition import OperatorDefinition
from py_rule.working_memory.trie_wm.parsing import FactParser as FP
from py_rule.abstract.parsing import util as PU


class TypeDef_ParserTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        FP.HOTLOAD_ANNOTATIONS << TP.TYPEDEC_CORE
        TP.HOTLOAD_BASIC_SEN << FP.BASIC_SEN
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
        self.assertIsNotNone(TD.RECORD_TYPE)
        self.assertIsNotNone(TD.SUM_TYPE)
        self.assertIsNotNone(TD.TYPE_CLASS_DEF)
        self.assertIsNotNone(TD.OP_DEF)

    def test_basic_typedef(self):
        result = TD.parseString("blah.x: (::σ)\na.b.c\n\nend")[0]
        self.assertIsInstance(result[-1]._value, TypeDefinition)
        self.assertEqual(result[-1].is_var, False)
        self.assertEqual(result[-1]._data[util.VALUE_TYPE_S], TU.TYPE_DEF_S)
        self.assertEqual(len(result[-1]._value._vars), 0)
        self.assertEqual(result[-1].name, "x")
        self.assertEqual(len(result[-1]._value.structure), 1)

    def test_typedef_with_variable(self):
        result = TD.parseString('blah.structure: (::σ)\n |$x |\n\na.b.$x\n\nend')[0]
        self.assertIsInstance(result[-1]._value, TypeDefinition)
        self.assertEqual(len(result[-1]._value._vars), 1)
        self.assertEqual(result[-1]._value._vars[0], "x")

    def test_typedef_with_multi_variables(self):
        result = TD.parseString('blah.x: (::σ)\n | $x, $y |\n\na.b.$x, a.b.$y\n\nend')[0]
        self.assertIsInstance(result[-1]._value, TypeDefinition)
        self.assertEqual(len(result[-1]._value._vars), 2)
        var_set = set([x for x in result[-1]._value._vars])
        match_set = set(["x", "y"])
        self.assertEqual(var_set, match_set)

    def test_typedef_with_structure_types(self):
        result = TD.parseString('blah.x: (::σ)\na.b.c(::bloo)\n\nend')[0]
        self.assertEqual(result[-1]._value.structure[0][-1]._data[TU.TYPE_DEC_S]._value.pprint(leaf=True), 'bloo')

    def test_typedef_with_bad_vars(self):
        with self.assertRaises(PyRuleParseException):
            result = TD.parseString('blah.x: (::σ)\n| $x |\n\na.b.c\n\nend')[0]

    def test_op_def_parse(self):
        the_string = 'AddOp: (::λ) $x(::Num).$x.$x => +'
        result = TD.OP_DEF.parseString(the_string)[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[-1]._value, OperatorDefinition)

    def test_op_def_parse_no_sugar(self):
        the_string = 'AddOp: (::λ) $x(::Num).$x.$x'
        result = TD.OP_DEF.parseString(the_string)[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[-1]._value, OperatorDefinition)


if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.trie_typedef_parser"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
