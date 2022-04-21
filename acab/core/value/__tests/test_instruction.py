#https://docs.python.org/3/library/unittest.html
from __future__ import annotations

import abc
import logging as logmod
import unittest
import unittest.mock as mock
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping, Type, NewType,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

logging = logmod.getLogger(__name__)
from acab import setup
from acab import types as AT

config = setup()

from acab.core.value import instruction as PO
from acab.core.value import default_structure as DS
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
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



class InstructionTests(unittest.TestCase):
    """ Test the construction of production abstractions """

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        logmod.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = logmod.StreamHandler()
        console.setLevel(logmod.INFO)
        logmod.getLogger('').addHandler(console)
        logging = logmod.getLogger(__name__)

    #----------
    def test_init_operator(self):
        """ Check an operator can be created, and is of the correct type """
        op = BasicTestOp()
        self.assertIsInstance(op, PO.ProductionOperator)
        # TODO OPERATOR, COMPONENT
        self.assertEqual(op.type, Sentence([OPERATOR_TYPE_PRIM_S]))
        self.assertEqual(op.name, f"{BasicTestOp.__qualname__}")

    def test_tagged_operator(self):
        op = BasicTestOp(tags=["a","b","c"])
        self.assertTrue(op.has_tag(AcabValue("a"), AcabValue("b"), AcabValue("c")))


