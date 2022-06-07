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

logging = logmod.getLogger(__name__)
import warnings

import acab
from acab import types as AT

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

import acab.core.defaults.value_keys as DS
from acab.core.value import instruction as PO
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.interfaces.value import Instruction_i, Sentence_i, Value_i

BIND_S               = DS.BIND
OPERATOR_TYPE_PRIM_S = config.attr.Type.Primitive.OPERATOR_PRIM

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



class ComponentTests(unittest.TestCase):
    """ Test the construction of production abstractions """

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

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    #----------
    def test_init_operator(self):
        """ Check an operator can be created, and is of the correct type """
        op = BasicTestOp()
        self.assertIsInstance(op, PO.ProductionOperator)
        self.assertEqual(op.type, Sentence([OPERATOR_TYPE_PRIM_S]))
        self.assertEqual(op.name, f"{BasicTestOp.__qualname__}")

    def test_tagged_operator(self):
        op = BasicTestOp(tags=["a","b","c"])
        self.assertTrue(op.has_tag(AcabValue("a"), AcabValue("b"), AcabValue("c")))


    def test_component_init(self):
        """ Check a component can be created """
        sen = Sentence(['testop'])
        val = PO.ProductionComponent(sen)
        self.assertIsInstance(val, Value_i)
        self.assertIsInstance(val.value, Sentence_i)
        self.assertIsInstance(val, Instruction_i)

    def test_component_with_params(self):
        """ Check a component can be created with parameters """
        val = PO.ProductionComponent(Sentence(["testop"]), params=[AcabValue("a"), AcabValue("b")])
        self.assertIsInstance(val, Value_i)
        self.assertIsInstance(val, Instruction_i)
        self.assertEqual(len(val.params), 2)

    def test_component_with_str_params(self):
        """ Check a component can be created with string parameters """
        val = PO.ProductionComponent(Sentence(["testop"]), params=["a", "b"])
        self.assertIsInstance(val, Value_i)
        self.assertIsInstance(val, Instruction_i)
        self.assertEqual(len(val.params), 2)
        self.assertTrue(all([isinstance(x, Value_i) for x in val.params]))


    def test_component_op(self):
        """ Test a components operator can be recovered """
        val = PO.ProductionComponent(Sentence(["testop"]))
        self.assertEqual(val.op, Sentence(["testop"]))


    def test_apply_parameters(self):
        """ Test a component can have parameters applied to it, creating a new component """
        val = PO.ProductionComponent(Sentence(["testop"]))
        self.assertEqual(len(val.params), 0)
        copied = val.apply_params(*["a","test"])
        self.assertNotEqual(val, copied)
        self.assertEqual(len(val.params), 0)
        self.assertEqual(len(copied.params), 2)
        self.assertNotEqual(val.uuid, copied.uuid)
