"""
Check the parser for assigning and describing types works
"""
from __future__ import annotations

import logging as logmod
import unittest
import warnings
from os.path import join, split, splitext
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    from acab.core.parsing import pyparse_dsl as ppDSL
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     import pyparsing as pp
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    from acab.core.parsing.annotation import ValueAnnotation
    import acab.core.defaults.value_keys as DS
    from acab.core.value.sentence import Sentence

    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.core.parsing.component_dsl import Component_DSL

    from acab.modules.analysis.typing.module import TypeSpecFragment
    from acab.modules.analysis.typing import util as TYU
    from acab.modules.analysis.typing.parsing import TypeDefParser as TDP
    from acab.modules.analysis.typing.parsing import TypeParser as TP
    from acab.modules.analysis.typing.values import (OperatorDefinition,
                                                    SumTypeDefinition,
                                                    TypeClass,
                                                    TypeDefinition)
    from acab.modules.analysis.typing import util

class TestParser(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.register(TypeSpecFragment().build_dsl())
        cls.dsl.build()
        # dsl()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)


    def test_single_line_typedef(self):
        """ Test a single line type definition  """
        result = TDP.NOMINAL_DEF.parse_string("c(::τ)")[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result.structure), 0)
        self.assertEqual(result.type, util.TYPE_DEFINITION)

    def test_simple_record_typedef(self):
        """ Test a multi line type definition """
        result = TDP.RECORD_TYPE.parse_string("c(::σ):\nq.w.e(::blah)\nend")[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result.structure), 1)
        self.assertEqual(result.type, util.TYPE_DEFINITION)

    def test_multi_val_record_typdef(self):
        """ Test a type definition with multiple subtypings """
        result = TDP.RECORD_TYPE.parse_string("c(::σ):\n q.w.e(::awef)\n blah.bloo(::awg)\nend")[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result.structure), 2)
        self.assertEqual(result.type, util.TYPE_DEFINITION)

    def test_simple_sum_type(self):
        """ Test a simple sum type definition """
        result = TDP.SUM_TYPE.parse_string("c(::Σσ):\n q(::τ)\n l(::τ)\nend")[0]
        self.assertIsInstance(result, SumTypeDefinition)
        self.assertEqual(len(result.structure), 2)
        self.assertIsInstance(result.structure[0], Sentence)
        self.assertIsInstance(result.structure[0][-1], TypeDefinition)
        self.assertEqual(result.type, util.SUM_DEFINITION)

    def test_nested_record_in_sum(self):
        """ Test nested record type definitions in a sum type """
        result = TDP.SUM_TYPE.parse_string("c(::Σσ):\n internal(::σ):\n q.w.e\n w.e.r\n end\n q.w.e(::τ)\n j.k.l(::τ)\nend")[0]
        self.assertIsInstance(result, SumTypeDefinition)
        self.assertEqual(len(result.structure), 3)
        self.assertIsInstance(result.structure[0], Sentence)
        self.assertIsInstance(result.structure[0][-1], TypeDefinition)
        self.assertEqual(result.type, util.SUM_DEFINITION)


    def test_simple_operator_def(self):
        """ Test an operator type definition """
        result = TDP.OP_DEF.parse_string("c(::λ): $x")[0]
        self.assertIsInstance(result, OperatorDefinition)
        self.assertEqual(result.type, util.OPERATOR_DEFINITION)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0][0], "_:[x]")
        self.assertEqual(result[0], "_:[[x]].[returns.unit]")

    def test_multi_param_def(self):
        result = TDP.OP_DEF.parse_string("c(::λ): $x $y $z")[0]
        self.assertIsInstance(result, OperatorDefinition)
        self.assertEqual(result.type, util.OPERATOR_DEFINITION)
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]), 2)
        self.assertEqual(result[0], "_:[[x].[y].[z]].[returns.unit]")

    def test_op_def_return(self):
        result = TDP.OP_DEF.parse_string("c(::λ): $x $y $z -> $a")[0]
        self.assertIsInstance(result, OperatorDefinition)
        self.assertEqual(result.type, util.OPERATOR_DEFINITION)
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]), 2)
        self.assertEqual(result[0], "_:[[x].[y].[z]].[returns.a]")

    def test_op_type_specs(self):
        result = TDP.OP_DEF.parse_string("c(::λ): $x(::blah) $y(::bloo) -> $z(::awef)")[0]
        self.assertIsInstance(result, OperatorDefinition)
        self.assertEqual(result.type, util.OPERATOR_DEFINITION)
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]), 2)
        self.assertEqual(result[0], "_:[[x].[y]].[returns.z]")
        self.assertEqual(result[0][0][0].type, "_:SENTENCE")
        self.assertEqual(result[0][0][1].type, "_:SENTENCE")
        self.assertEqual(result[0][1][-1].type, "_:awef")

    def test_op_multi_spec(self):
        result = TDP.MULTI_OP_DEF.parse_string("c(::λ):\n  $x(::blah) $y(::bloo) -> $z(::awef)\n  $x -> $y\nend")[0]
        self.assertIsInstance(result, OperatorDefinition)
        self.assertEqual(result.type, util.OPERATOR_DEFINITION)
        self.assertEqual(len(result), 2)
        self.assertEqual(len(result[0]), 2)
        self.assertEqual(result[0], "_:[[x].[y]].[returns.z]")
        self.assertEqual(result[0][0][0].type, "_:SENTENCE")
        self.assertEqual(result[0][0][1].type, "_:SENTENCE")
        self.assertEqual(result[0][1][1].type, "_:awef")
        self.assertEqual(result[1], "_:[[x]].[returns.y]")

    def test_simple_operator_def_fail(self):
        """ Test an operator type definition without variable params """
        with self.assertRaises(AssertionError):
            TDP.OP_DEF.parse_string("c(::λ): q w e")[0]

    def test_simple_type_class(self):
        """ Test a type class definition """
        result = TDP.TYPE_CLASS_DEF.parse_string("class(::γ):\n a.b.c(::λ): $q $w $e \n q.w.e(::λ): $w $e $r \nend")[0]
        self.assertIsInstance(result, TypeClass)
        self.assertTrue(len(result.structure), 2)
        self.assertEqual(result.type, util.TYPE_CLASS)


    def test_simple_type_declaration(self):
        """ Test a type declaration"""
        result = TP.TYPEDEC_CORE.parse_string("::τ")[0]
        self.assertIsInstance(result, ValueAnnotation)
        self.assertEqual(result.key, DS.TYPE_INSTANCE)
        self.assertIsInstance(result.value, Sentence)


    def test_type_declaration_sentence(self):
        """ Test a type declaration"""
        result = TP.TYPEDEC_CORE.parse_string("::a.b.c")[0]
        self.assertIsInstance(result, ValueAnnotation)
        self.assertEqual(result.key, DS.TYPE_INSTANCE)
        self.assertIsInstance(result.value, Sentence)
        self.assertTrue(result.value == "_:a.b.c")


    def test_declaration_as_annotation(self):
        """ Test a sentence with a type declaration as an annotation """
        result = self.dsl("a.test(::blah.type)")[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(result[-1].type, "_:blah.type")



if __name__ == '__main__':
    unittest.main()
