#!/opts/anaconda3/envs/ENV/python
"""
A Bare minimum template for testing with a set up acab engine
"""
import logging as logmod
import unittest
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock
from unittest.mock import create_autospec
import warnings

logging = logmod.getLogger(__name__)

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.printing.default import DEFAULT_PRINTER
from acab.modules.semantics.default import DEFAULT_SEMANTICS


class _ENGINE_TEST_TEMPLATE(unittest.TestCase):

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

        cls.eng = AcabBasicEngine(parser=EXLO_Parser,
                                  semantics=DEFAULT_SEMANTICS(),
                                  printer=DEFAULT_PRINTER(),
                                  modules=[])

    @classmethod
    def tearDownClass(cls):
        logging.root.removeHandler(cls.file_h)
