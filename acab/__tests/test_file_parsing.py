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
from functools import partial
import timeit

logging = logmod.getLogger(__name__)

import acab

config = acab.setup()

from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.printing.default import DEFAULT_PRINTER
from acab.modules.semantics.default import DEFAULT_SEMANTICS

def parse_file_runner(eng, f):
    eng.load_file(f)


class FileParsingTests(unittest.TestCase):


    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.WARNING
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        cls.eng = AcabBasicEngine(parser=EXLO_Parser,
                                  semantics=DEFAULT_SEMANTICS(),
                                  printer=DEFAULT_PRINTER(),
                                  modules=[])

    def test_file_parsing_times(self):
        test_loc = join(split(__file__)[0], "test_files")
        test_files = [join(test_loc, x) for x in listdir(test_loc)
                      if isfile(join(test_loc, x)) and splitext(x)[1] == ".trie"]

        for file_name in test_files:
            logging.warning(f"\nTrying: {file_name}")
            test_func = partial(parse_file_runner, self.eng, file_name)
            t = timeit.timeit(test_func, number=1)
            with open(file_name) as f:
                lines = len(f.readlines())

            logging.warning("Length: {}", lines)
            logging.warning("Time  : {:.4}", t)
