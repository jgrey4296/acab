#!/usr/bin/env python3
import logging as logmod
import unittest
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock

logging = logmod.getLogger(__name__)

import warnings
import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    acab.setup()
    from acab.core.engine.module_loader import ModuleLoader


class TestModuleLoader(unittest.TestCase):

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


    def test_initial_ModuleLoader(self):
        ml = ModuleLoader()
        self.assertIsNotNone(ml)

    def test_load_module(self):
        ml = ModuleLoader()
        result = ml.load("acab.core.engine.__tests.test_module")
        self.assertTrue(result)
        self.assertEqual(len(result[0].dsl_fragments), 1)
        self.assertEqual(len(result[0].semantics), 1)
        self.assertEqual(len(result[0].printers), 1)
        self.assertEqual(len(result[0].operators), 0)

    def test_load_module_explicit(self):
        ml = ModuleLoader()
        result = ml.load("acab.core.engine.__tests.test_module.module")
        self.assertTrue(result)
        self.assertEqual(len(result[0].dsl_fragments), 1)
        self.assertEqual(len(result[0].semantics), 1)
        self.assertEqual(len(result[0].printers), 1)
        self.assertEqual(len(result[0].operators), 0)


    def test_load_alt_module(self):
        ml = ModuleLoader()
        result = ml.load("acab.core.engine.__tests.test_module.alt_mod")
        self.assertTrue(result)
        self.assertEqual(len(result[0].dsl_fragments), 1)
        self.assertEqual(len(result[0].semantics), 0)
        self.assertEqual(len(result[0].printers), 0)
        self.assertEqual(len(result[0].operators), 0)

    def test_load_module_separates(self):
        ml = ModuleLoader()
        result = ml.load("acab.core.engine.__tests.test_module.separates")
        self.assertTrue(result)
        self.assertEqual(len(result[0].dsl_fragments), 1)
        self.assertEqual(len(result[0].semantics), 1)
        self.assertEqual(len(result[0].printers), 0)
        self.assertEqual(len(result[0].operators), 0)

    def test_load_module_without_all_defined(self):
        ml = ModuleLoader()
        result = ml.load("acab.core.engine.__tests.test_module.no_all")
        self.assertTrue(result)
        self.assertEqual(len(result[0].dsl_fragments), 1)
        self.assertEqual(len(result[0].semantics), 0)
        self.assertEqual(len(result[0].printers), 1)
        self.assertEqual(len(result[0].operators), 2)

    def test_load_module_twice(self):
        ml = ModuleLoader()
        result = ml.load("acab.core.engine.__tests.test_module.no_all")
        result = ml.load("acab.core.engine.__tests.test_module.no_all")
        pass

if __name__ == '__main__':
    unittest.main()
