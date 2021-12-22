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
from acab.core.parsing.annotation import ValueAnnotation
from acab.core.data import default_structure as DS

from acab.core.data.value import Sentence
from acab.modules.analysis.typing.values.type_definition import TypeDefinition, SumTypeDefinition, OperatorDefinition, TypeClass

TDP.HOTLOAD_SEN        << FP.SENTENCE
TP.HOTLOAD_SEN         << FP.SENTENCE
FP.HOTLOAD_ANNOTATIONS << TP.TYPEDEC_CORE
FP.HOTLOAD_SEN_ENDS    << TDP.COMBINED_DEFS

class TestParser(unittest.TestCase):

    def test_single_line_typedef(self):
        """ Test a single line type definition  """
        result = TDP.NOMINAL_DEF.parseString("c(::τ)")[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result.structure), 0)

    def test_simple_record_typedef(self):
        """ Test a multi line type definition """
        result = TDP.RECORD_TYPE.parseString("c(::τ):\nq.w.e(::blah)\nend")[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result.structure), 1)

    def test_multi_val_record_typdef(self):
        """ Test a type definition with multiple subtypings """
        result = TDP.RECORD_TYPE.parseString("c(::τ):\nq.w.e(::awef)\nblah.bloo(::awg)\nend")[0]
        self.assertIsInstance(result, TypeDefinition)
        self.assertEqual(len(result.structure), 2)

    def test_simple_sum_type(self):
        """ Test a simple sum type definition """
        result = TDP.SUM_TYPE.parseString("c(::Σ):\n q(::τ)\n l(::τ)\nend")[0]
        self.assertIsInstance(result, SumTypeDefinition)
        self.assertEqual(len(result.structure), 2)
        self.assertIsInstance(result.structure[0], Sentence)
        self.assertIsInstance(result.structure[0][-1], TypeDefinition)

    def test_nested_record_in_sum(self):
        """ Test nested record type definitions in a sum type """
        result = TDP.SUM_TYPE.parseString("c(::Σ):\n internal(::τ):\n q.w.e\n w.e.r\n end\n q.w.e(::τ)\n j.k.l(::τ)\nend")[0]
        self.assertIsInstance(result, SumTypeDefinition)
        self.assertEqual(len(result.structure), 3)
        self.assertIsInstance(result.structure[0], Sentence)
        self.assertIsInstance(result.structure[0][-1], TypeDefinition)

    def test_simple_operator_def(self):
        """ Test an operator type definition """
        result = TDP.OP_DEF.parseString("c(::λ): $q.$w.$e => +")[0]
        self.assertIsInstance(result, OperatorDefinition)


    def test_simple_operator_def_fail(self):
        """ Test an operator type definition without variable params """
        with self.assertRaises(AssertionError):
            TDP.OP_DEF.parseString("c(::λ): q.w.e => +")[0]


    def test_simple_type_class(self):
        """ Test a type class definition """
        result = TDP.TYPE_CLASS_DEF.parseString("class(::ι):\n a.b.c(::λ): $q.$w.$e => +\n q.w.e(::λ): $w.$e.$r => -\nend")[0]
        self.assertIsInstance(result, TypeClass)
        self.assertTrue(len(result.structure), 2)

    def test_simple_type_declaration(self):
        """ Test a type declaration"""
        result = TP.TYPEDEC_CORE.parseString("::τ")[0]
        self.assertIsInstance(result, ValueAnnotation)
        self.assertEqual(result.key, DS.TYPE_INSTANCE)
        self.assertIsInstance(result.value, Sentence)


    def test_type_declaration_sentence(self):
        """ Test a type declaration"""
        result = TP.TYPEDEC_CORE.parseString("::a.b.c")[0]
        self.assertIsInstance(result, ValueAnnotation)
        self.assertEqual(result.key, DS.TYPE_INSTANCE)
        self.assertIsInstance(result.value, Sentence)
        self.assertTrue(str(result.value) == "a.b.c")


    def test_declaration_as_annotation(self):
        """ Test a sentence with a type declaration as an annotation """
        result = FP.SENTENCE.parseString("a.test(::blah.type)")[0]
        self.assertIsInstance(result, Sentence)
        self.assertEqual(str(result[-1].type), "blah.type")



if __name__ == '__main__':
    unittest.main()
