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



class StatementTests(unittest.TestCase):
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
    def test_init_operator(self):
        """ Check an operator can be created, and is of the correct type """
        op = BasicTestOp()
        self.assertIsInstance(op, PO.ProductionOperator)
        # TODO OPERATOR, COMPONENT
        self.assertEqual(op.type, Sentence([OPERATOR_TYPE_PRIM_S]))
        self.assertEqual(op.name, f"{BasicTestOp.__module__}.{BasicTestOp.__qualname__}")

    def test_tagged_operator(self):
        op = BasicTestOp(tags=["a","b","c"])
        self.assertTrue(op.has_tag(AcabValue("a"), AcabValue("b"), AcabValue("c")))


    def test_component_init(self):
        """ Check a component can be created """
        val = PO.ProductionComponent(Sentence(["testop"]))
        self.assertIsInstance(val, Value_i)

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


    def test_container_init(self):
        val = PO.ProductionContainer([])
        self.assertIsInstance(val, Instruction_i)
        self.assertIsInstance(val, PO.ProductionContainer)
        self.assertEqual(val.type, Sentence([DS.CONTAINER_PRIM]))


    def test_structure_init(self):
        val = PO.ProductionStructure({"test": "a", "struct": "b"})
        self.assertIsInstance(val, Instruction_i)
        self.assertIsInstance(val, PO.ProductionContainer)
        self.assertEqual(val.type, Sentence([DS.STRUCT_PRIM]))


    def test_component_op(self):
        """ Test a components operator can be recovered """
        val = PO.ProductionComponent(Sentence(["testop"]))
        self.assertEqual(val.op, Sentence(["testop"]))


    def test_apply_parameters(self):
        """ Test a component can have parameters applied to it, creating a new component """
        val = PO.ProductionComponent(Sentence(["testop"]))
        self.assertEqual(len(val.params), 0)
        copied = val.apply_params(["a","test"])
        self.assertNotEqual(val, copied)
        self.assertEqual(len(val.params), 0)
        self.assertEqual(len(copied.params), 2)
        self.assertNotEqual(val.uuid, copied.uuid)


    def test_component_to_sentences(self):
        sen = Sentence(["Test", "Path", "Op"])
        comp = PO.ProductionComponent(sen,
                                            params=[AcabValue("x"),
                                                    AcabValue("y")],
                                            rebind=AcabValue("blah"))

        as_sen = comp.to_sentences()
        self.assertEqual(as_sen, "ProductionComponent")
        self.assertIn('Operator', as_sen)
        self.assertEqual(as_sen['Operator'], "_:Test.Path.Op")
        self.assertIn('Params', as_sen)
        self.assertEqual(as_sen['Params'], "_:x.y")
        self.assertIn('Rebind', as_sen)

    def test_component_to_sentences_no_params(self):
        comp = PO.ProductionComponent(Sentence(["Test.Op.Path"]))

        as_sen = comp.to_sentences()
        self.assertEqual(as_sen, "ProductionComponent")
        self.assertEqual(as_sen[0], "Operator")
        self.assertEqual(as_sen['Operator'], "_:Test.Op.Path")

    def test_component_from_sentences(self):
        comp = PO.ProductionComponent(Sentence(["Test", "Path", "Op"]),
                                      params=[AcabValue("x"),
                                              AcabValue("y")],
                                              rebind=AcabValue("blah"))

        as_sen = comp.to_sentences()
        comp2 = PO.ProductionComponent.from_sentences([as_sen])[0]
        self.assertEqual(comp.value, comp2.value)
        self.assertEqual(comp.params, comp2.params)
        self.assertEqual(comp.rebind, comp2.rebind)

    def test_multi_component_from_sentences(self):
        comp = PO.ProductionComponent(Sentence(["Test", "Path", "Op"]),
                                      params=[AcabValue("x"),
                                              AcabValue("y")],
                                              rebind=AcabValue("blah"))

        as_sen = comp.to_sentences()
        as_sen2 = comp.to_sentences()

        comp2 = PO.ProductionComponent.from_sentences([as_sen, as_sen2])
        self.assertEqual(len(comp2), 2)
        self.assertEqual(comp.value, comp2[1].value)
        self.assertEqual(comp.params, comp2[1].params)
        self.assertEqual(comp.rebind, comp2[1].rebind)

    @unittest.skip
    def test_container_to_sentences(self):
        pass
