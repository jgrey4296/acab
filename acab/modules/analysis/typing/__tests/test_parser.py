import unittest
from unittest import mock
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import acab
config = acab.setup()

from acab.modules.parsing.exlo.parsers import FactParser as FP
from acab.modules.analysis.typing.parsing import TypeDefParser as TDP
from acab.modules.analysis.typing.parsing import TypeParser as TP
from acab.modules.analysis.typing import util as TYU
from acab.abstract.parsing.annotation import ValueAnnotation
from acab.abstract.core import default_structure as DS

from acab.abstract.core.values import Sentence
from acab.modules.analysis.typing.values.type_definition import TypeDefinition, SumTypeDefinition, OperatorDefinition, TypeClass

TDP.HOTLOAD_BASIC_SEN << FP.BASIC_SEN
TDP.HOTLOAD_PARAM_SEN << FP.PARAM_SEN
TP.HOTLOAD_BASIC_SEN  << FP.BASIC_SEN
FP.HOTLOAD_ANNOTATIONS << TP.TYPEDEC_CORE

class TestParser(unittest.TestCase):

    def test_single_line_typedef(self):
        """ Test a single line type definition  """
        result = TDP.SIMPLE_DEF.parseString("τ::a.b.c")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsInstance(result[-1], TypeDefinition)
        self.assertEqual(len(result[-1].structure), 0)

    def test_simple_typedef(self):
        """ Test a multi line type definition """
        result = TDP.RECORD_TYPE.parseString("τ::a.b.c:\nq.w.e(::blah)\nend")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsInstance(result[-1], TypeDefinition)
        self.assertEqual(len(result[-1].structure), 1)

    def test_multi_val_typdef(self):
        """ Test a type definition with multiple subtypings """
        result = TDP.RECORD_TYPE.parseString("τ::a.b.c:\nq.w.e(::awef)\nblah.bloo(::awg)\nend")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsInstance(result[-1], TypeDefinition)
        self.assertEqual(len(result[-1].structure), 2)

    def test_simple_sum_type(self):
        """ Test a simple sum type definition """
        result = TDP.SUM_TYPE.parseString("Σ::a.b.c:\n τ::q.w.e\n τ::j.k.l\nend")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsInstance(result[-1], SumTypeDefinition)
        self.assertEqual(len(result[-1].structure), 2)
        self.assertIsInstance(result[-1].structure[0], Sentence)
        self.assertIsInstance(result[-1].structure[0][-1], TypeDefinition)

    def test_nested_record_in_sum(self):
        """ Test nested record type definitions in a sum type """
        result = TDP.SUM_TYPE.parseString("Σ::a.b.c:\n τ::internal.type:\n q.w.e\n w.e.r\n end\n τ::q.w.e\n τ::j.k.l\nend")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsInstance(result[-1], SumTypeDefinition)
        self.assertEqual(len(result[-1].structure), 3)
        self.assertIsInstance(result[-1].structure[0], Sentence)
        self.assertIsInstance(result[-1].structure[0][-1], TypeDefinition)

    def test_simple_operator_def(self):
        """ Test an operator type definition """
        result = TDP.OP_DEF.parseString("λ::a.b.c: q.w.e => +")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsInstance(result[-1], OperatorDefinition)


    def test_simple_type_class(self):
        """ Test a type class definition """
        result = TDP.TYPE_CLASS_DEF.parseString("γ::a.type.class:\n λ::a.b.c: q.w.e => +\n λ::q.w.e: w.e.r => -\nend")[0]
        self.assertIsInstance(result, Sentence)
        self.assertIsInstance(result[-1], TypeClass)
        self.assertTrue(len(result[-1].structure), 2)

    def test_simple_type_declaration(self):
        """ Test a type declaration"""
        result = TP.TYPEDEC_CORE.parseString("::a.test.type")[0]
        self.assertIsInstance(result, ValueAnnotation)
        self.assertEqual(result.key, DS.TYPE_INSTANCE)
        self.assertIsInstance(result.value, Sentence)


    def test_declaration_as_annotation(self):
        """ Test a sentence with a type declaration as an annotation """
        result = FP.PARAM_SEN.parseString("a.test(::blah.type)")[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(str(result[-1].type), "_:blah.type")



if __name__ == '__main__':
    unittest.main()
