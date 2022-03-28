#https://docs.python.org/3/library/unittest.html
from __future__ import annotations

import abc
import logging as root_logger
import unittest
import unittest.mock as mock
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping, Type, NewType,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

logging = root_logger.getLogger(__name__)
from acab import setup
from acab import types as AT

config = setup()

from acab.core.data import instruction as PO
from acab.core.data import default_structure as DS
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.core.data.value import AcabValue
from acab.interfaces.value import Instruction_i, Sentence_i, Value_i

BIND_S               = config.prepare("Value.Structure", "BIND")()
OPERATOR_TYPE_PRIM_S = config.prepare("Type.Primitive", "OPERATOR")()

T     = TypeVar('T')
Value_A       : TypeAlias = "AT.Value[AT.ValueCore]"
Value_t       : TypeAlias = Type[Value_A]
Sen_A         : TypeAlias = AT.Sentence
Sen_t         : TypeAlias = Type[Sen_A]
Instruction_A : TypeAlias = AT.Instruction
Variable      = NewType('Variable', Value_A)
ValueData     : TypeAlias = str
GenFunc       : TypeAlias = AT.fns.GenFunc
SemSys_A      : TypeAlias = AT.SemanticSystem


class BasicTestOp(PO.ProductionOperator):

    def __call__(self, *params: Value_A, data:None|dict[str,Any]=None) -> str:
        return "test"

class BasicTestAct(PO.ActionOperator):
    val : ClassVar[int] = 0

    def __call__(self, *params: Value_A, data:None|dict[str,Any]=None, semsys:None|SemSys_A=None) -> None:
        BasicTestAct.val = 5



class StructureTests(unittest.TestCase):
    """ Test the construction of production abstractions """

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    #----------
    def test_structure_init(self):
        val = PO.ProductionStructure(["test", "blah"])
        self.assertIsInstance(val, Instruction_i)
        self.assertIsInstance(val, PO.ProductionContainer)
        self.assertIsInstance(val, PO.ProductionStructure)
        self.assertEqual(val.type, Sentence([DS.STRUCT_PRIM]))


    def test_structure_contains(self):
        val = PO.ProductionStructure(["test", "blah"])
        self.assertIsInstance(val, PO.ProductionStructure)
        self.assertTrue("test" in val)
        self.assertTrue("blah" in val)


    def test_structure_get_items(self):
        val = PO.ProductionStructure(["test", "blah"])
        self.assertIsInstance(val, PO.ProductionStructure)
        self.assertIsInstance(val['test'], PO.ProductionContainer)
        self.assertIsInstance(val['blah'], PO.ProductionContainer)


