#!/opts/anaconda3/envs/ENV/python
import logging as root_logger
import unittest
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock
from unittest.mock import create_autospec

logging = root_logger.getLogger(__name__)

import acab

config = acab.setup()

from acab.abstract.interfaces.engine_interface import AcabEngine_Interface
from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.modules.parsing.exlo.el_dsl import EL_Parser
from acab.modules.printing.basic_printer import BasicPrinter
from acab.modules.printing.default import DEFAULT_PRINTER
from acab.modules.semantics.default import DEFAULT_SEMANTICS
from acab.modules.semantics.system import BasicSemanticSystem


class TestEngine(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    def test_basic(self):
        """ Test the engine can be created in the most basic way"""
        parser    = create_autospec(EL_Parser)
        parser.query_parsers = mock.Mock(return_value=(1,2))
        semantics = create_autospec(BasicSemanticSystem)
        printer   = create_autospec(BasicPrinter)
        basic     = AcabBasicEngine(parser=parser,
                                    semantics=semantics,
                                    printer=printer,
                                    modules=[])

        self.assertIsInstance(basic, AcabEngine_Interface)
        self.assertTrue(basic.initialised)

        parser.assert_parsers.assert_called_once()
        parser.query_parsers.assert_called_once()

    def test_parser_setup(self):
        # Create the engine
        basic     = AcabBasicEngine(parser=EL_Parser(),
                                    semantics=DEFAULT_SEMANTICS,
                                    printer=DEFAULT_PRINTER,
                                    modules=[])

        query = basic.query("a.test.sentence?")
        self.assertFalse(bool(query))
        basic.insert("a.test.sentence")
        result = basic.query("a.test.sentence?")
        self.assertTrue(bool(result))

    def test_printer_setup(self):
        # Create the engine
        basic     = AcabBasicEngine(parser=EL_Parser(),
                                    semantics=DEFAULT_SEMANTICS,
                                    printer=DEFAULT_PRINTER,
                                    modules=[])

        basic.insert("a.test.sentence")
        basic.insert("a.test.blah")
        sentences = basic.to_sentences()
        self.assertEqual(len(sentences), 2)
        result = basic.pprint()
        self.assertEqual(result, "a.test.sentence\na.test.blah")

    def test_module_setup(self):
        pass
