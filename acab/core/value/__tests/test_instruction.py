#https://docs.python.org/3/library/unittest.html
from __future__ import annotations

import abc
import logging as logmod
import unittest
import unittest.mock as mock
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    NewType, Protocol, Sequence, Tuple, Type, TypeAlias,
                    TypeGuard, TypeVar, cast, final, overload,
                    runtime_checkable)
import warnings

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    import acab
    from acab import types as AT
    config = acab.setup()

    import acab.core.defaults.value_keys as DS
    from acab.core.value import instruction as PO
    from acab.core.value.instruction import Instruction
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.value import Instruction_i, Sentence_i, Value_i

if TYPE_CHECKING:
    Value_A       : TypeAlias = "AT.Value[AT.ValueCore]"
    Value_t       : TypeAlias = Type[Value_A]
    Sen_A         : TypeAlias = AT.Sentence
    Sen_t         : TypeAlias = Type[Sen_A]
    Instruction_A : TypeAlias = AT.Instruction
    Variable      = NewType('Variable', Value_A)
    ValueData     : TypeAlias = str
    GenFunc       : TypeAlias = AT.fns.GenFunc
    SemSys_A      : TypeAlias = AT.SemanticSystem

T                    = TypeVar('T')
BIND_S               = config.attr.Value.Structure.BIND
OPERATOR_TYPE_PRIM_S = config.attr.Type.Primitive.OPERATOR_PRIM



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
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

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


