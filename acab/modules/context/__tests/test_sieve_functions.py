#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html
from __future__ import annotations

import logging as logmod
import unittest
import warnings
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
from unittest import mock

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

class TestConstraintSieveFunctions(unittest.TestCase):

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


    def test_creation(self):
        pass

    def test_top_level_test(self):
        pass

    def test_constraint_sieve(self):
        pass
