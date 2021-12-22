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

from acab.interfaces.engine import AcabEngine_i
from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.printing.basic_printer import BasicPrinter
from acab.modules.printing.default import DEFAULT_PRINTER
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.default import DEFAULT_SEMANTICS
from acab.core.parsing import pyparse_dsl as ppDSL

DSL_Fragment = ppDSL.DSL_Fragment

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
        parser    = create_autospec(EXLO_Parser)
        parser.query_parsers = mock.Mock(return_value=(1,2))
        semantics = create_autospec(BasicSemanticSystem)
        printer   = create_autospec(BasicPrinter)
        basic     = AcabBasicEngine(parser=parser,
                                    semantics=semantics,
                                    printer=printer,
                                    modules=[])

        self.assertIsInstance(basic, AcabEngine_i)
        self.assertTrue(basic.initialised)

        parser.assert_parsers.assert_called_once()
        parser.query_parsers.assert_called_once()

    def test_parser_into_semantics(self):
        # Create the engine
        basic     = AcabBasicEngine(parser=EXLO_Parser(),
                                    semantics=DEFAULT_SEMANTICS(),
                                    printer=DEFAULT_PRINTER(),
                                    modules=[])

        query = basic("a.test.sentence?")
        self.assertFalse(bool(query))
        basic("a.test.sentence")
        result = basic("a.test.sentence?")
        self.assertTrue(bool(result))

    def test_query(self):
        basic = AcabBasicEngine(parser=EXLO_Parser(),
                                semantics=DEFAULT_SEMANTICS(),
                                printer=DEFAULT_PRINTER(),
                                modules=[])

        query = basic("a.test.$x?")
        self.assertFalse(bool(query))
        basic("a.test.sentence")
        result = basic("a.test.$x?")
        self.assertTrue(bool(result))
        self.assertEqual(result[0]['x'].value, 'sentence')

    def test_printer_setup(self):
        # Create the engine
        basic     = AcabBasicEngine(parser=EXLO_Parser(),
                                    semantics=DEFAULT_SEMANTICS(),
                                    printer=DEFAULT_PRINTER(),
                                    modules=[])

        basic("a.test.sentence")
        basic("a.test.blah")
        sentences = basic.semantics.to_sentences()
        self.assertEqual(len(sentences), 2)
        result = basic.pprint()
        self.assertEqual(result, "a.test.sentence\na.test.blah")

    @unittest.skip("need to change total parser priorities?")
    def test_multi_sentence_statement(self):
        # Create the engine
        basic     = AcabBasicEngine(parser=EXLO_Parser(),
                                    semantics=DEFAULT_SEMANTICS(),
                                    printer=DEFAULT_PRINTER(),
                                    modules=[])


        basic("a.test.sentence:\nblah\nbloo\nblee\nend")

        sentences = basic.semantics.to_sentences()
        self.assertEqual(len(sentences), 3)
        result = basic.pprint()
        self.assertEqual(result, "a.test.sentence.blah\na.test.sentence.bloo\na.test.sentence.blee")



    @unittest.skip("need to finish this")
    def test_module_setup(self):
        basic     = AcabBasicEngine(parser=EXLO_Parser(),
                                    semantics=DEFAULT_SEMANTICS(),
                                    printer=DEFAULT_PRINTER(),
                                    modules=["acab.modules.operators.query"])
        self.assertTrue("acab.modules.operators.query" in basic._module_loader)

        basic("a.test.sentence")
        result = basic("a.test.sentence(#test)?")

        breakpoint()
        self.assertTrue(bool(result))
