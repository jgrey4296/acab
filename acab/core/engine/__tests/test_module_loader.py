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
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)


    def test_initial_ModuleLoader(self):
        ml = ModuleLoader()
        self.assertIsNotNone(ml)

    def test_load_module(self):
        pass

    def test_extract_from_module(self):
        pass

    def test_contains(self):
        pass


if __name__ == '__main__':
    unittest.main()
