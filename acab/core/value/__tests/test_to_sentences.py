#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
import unittest
import unittest.mock as mock
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

import warnings


with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    import acab
    config = acab.setup()
    from acab.core.parsing import pyparse_dsl as ppDSL

from acab.interfaces import value as VI
import acab.core.defaults.value_keys as DS
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.core.value import instruction as PO
from acab.interfaces.value import Sentence_i, Value_i
from acab.interfaces.value import ValueFactory as VF
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser


class TestToSentenceReduction(unittest.TestCase):

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

    def test_value_to_sentences(self):
        pass

    def test_to_sentences(self):
        val = AcabValue("Test")
        as_sens = val.to_sentences()
        self.assertIsInstance(as_sens, list)
        self.assertEqual(len(as_sens), 1)
        self.assertEqual(len(as_sens[0]), 1)
        self.assertIsInstance(as_sens[0], VI.Sentence_i)
        self.assertEqual(as_sens[0], "_:Test")

    def test_to_sentences_var(self):
        val = AcabValue("Test", data={DS.BIND: True})
        as_sens = val.to_sentences()
        self.assertIsInstance(as_sens, list)
        self.assertEqual(len(as_sens), 1)
        self.assertEqual(len(as_sens[0]), 1)
        self.assertIsInstance(as_sens[0], VI.Sentence_i)
        self.assertEqual(as_sens[0], "_:Test")
        self.assertTrue(as_sens[0].has_var)

    def test_sentence_to_sentences(self):
        pass

    def test_sen_to_sentences(self):
        sen = Sentence() << "a" << "test" << "sentence"
        as_sens = sen.to_sentences()
        self.assertIsInstance(as_sens, list)
        self.assertEqual(len(as_sens), 3)
        self.assertEqual(len(as_sens[0]), 4)
        self.assertEqual(as_sens[1], sen)

    def test_sen_to_sentences_with_var(self):
        sen = Sentence() << "a" << VF.value("test", data={DS.BIND: True}) << "sentence"
        as_sens = sen.to_sentences()
        self.assertIsInstance(as_sens, list)
        self.assertEqual(len(as_sens), 3)
        self.assertEqual(len(as_sens[0]), 4)
        self.assertEqual(as_sens[1], sen)

    def test_prod_comp_to_sentences(self):
        pass

    def test_component_to_sentences(self):
        sen = Sentence(["Test", "Path", "Op"])
        comp = PO.ProductionComponent(sen,
                                      params=[AcabValue("x"),
                                              AcabValue("y")],
                                      rebind=AcabValue("blah"))

        as_sens = comp.to_sentences()
        self.assertIsInstance(as_sens, list)
        self.assertEqual(len(as_sens), 5)
        self.assertEqual(as_sens[0][0], "ProductionComponent")
        self.assertEqual(as_sens[-1][0], "end")
        self.assertEqual(as_sens[1], "_:operator.[Test.Path.Op]")
        self.assertEqual(as_sens[2], "_:params.x.y")
        self.assertEqual(as_sens[3], "_:rebind.blah")

    def test_component_to_sentences_no_params(self):
        comp = PO.ProductionComponent(Sentence(["Test.Op.Path"]))

        as_sens = comp.to_sentences()
        self.assertEqual(len(as_sens), 4)
        self.assertEqual(as_sens[0][0], "ProductionComponent")
        self.assertEqual(as_sens[-1][0], "end")
        self.assertEqual(as_sens[1], "_:operator.[Test.Op.Path]")
        self.assertEqual(as_sens[2], "_:params")

    @unittest.expectedFailure
    def test_component_from_sentences(self):
        comp = PO.ProductionComponent(Sentence(["Test", "Path", "Op"]),
                                      params=[AcabValue("x"),
                                              AcabValue("y")],
                                              rebind=AcabValue("blah"))

        as_sens = comp.to_sentences()
        self.assertEqual(as_sens[0][0], "ProductionComponent")
        self.assertEqual(as_sens[-1][0], "end")
        self.assertEqual(as_sens[1], "_:operator.[Test.Path.Op]")

        comp2 = PO.ProductionComponent.from_sentences(as_sens)[0]
        self.assertEqual(comp.value, comp2.value)
        self.assertEqual(comp.params, comp2.params)
        self.assertEqual(comp.rebind, comp2.rebind)

    @unittest.expectedFailure
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

    def test_prod_container_to_sentences(self):
        pass

    def test_prod_struct_to_sentences(self):
        pass



if __name__ == '__main__':
    unittest.main()
