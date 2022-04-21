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



class ContainerTests(unittest.TestCase):
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
    def test_container_init(self):
        val = PO.ProductionContainer([])
        self.assertIsInstance(val, Instruction_i)
        self.assertIsInstance(val, PO.ProductionContainer)
        self.assertEqual(val.type, Sentence([DS.CONTAINER_PRIM]))

    def test_container_bool(self):
        self.assertFalse(bool(PO.ProductionContainer([])))
        self.assertTrue(bool(PO.ProductionContainer(["a", "b"])))

    def test_container_contains(self):
        container = PO.ProductionContainer(["a", "b", "c"])
        self.assertTrue("b" in container)

    def test_container_sentence_contains(self):
        sen = Sentence(["a", "b", "c"])
        container = PO.ProductionContainer(["q", sen, "d"])
        self.assertTrue(sen in container)
        self.assertTrue("_:a.b.c" in container)
        sen_eqv = Sentence(["a", "b", "c"])
        self.assertTrue(sen_eqv in container)

    def test_container_iterator(self):
        container = PO.ProductionContainer(["a", "b", "c"])
        for x,y in zip(container, ["a","b","c"]):
            self.assertEqual(x,y)



    @unittest.skip
    def test_container_to_sentences(self):
        pass
