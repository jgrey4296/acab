#!/opts/anaconda3/envs/ENV/python
import unittest
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock
from unittest.mock import create_autospec

import acab

config = acab.setup()

from acab.abstract.interfaces.engine_interface import AcabEngine_Interface
from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.abstract.interfaces.printing_interfaces import PrintSystem
from acab.modules.parsing.exlo.el_dsl import EL_Parser
from acab.modules.semantics.system import BasicSemanticSystem

class TestEngine(unittest.TestCase):


    def test_basic(self):
        # TODO mock the module loader
        # TODO mock the DSL Builder
        parser    = create_autospec(EL_Parser)
        parser.query_parsers = mock.Mock(return_value=(1,2))
        semantics = BasicSemanticSystem(handlers=[], structs=[])
        printer   = PrintSystem(handlers=[], structs=[])
        basic     = AcabBasicEngine(parser=parser,
                                    semantics=semantics,
                                    printer=printer,
                                    modules=[])

        self.assertIsInstance(basic, AcabEngine_Interface)
        self.assertTrue(basic.initialised)
        # TODO assert load_modules is called,
        # TODO assert build_DSL is called

        parser.assert_parsers.assert_called_once()
        parser.query_parsers.assert_called_once()

    def test_parser_setup(self):
        parser    = create_autospec(EL_Parser)
        parser.query_parsers = mock.Mock(return_value=(1,2))
        semantics = BasicSemanticSystem(handlers=[], structs=[])
        printer   = PrintSystem(handlers=[], structs=[])
        basic     = AcabBasicEngine(parser=parser,
                                    semantics=semantics,
                                    printer=printer,
                                    modules=[])


        pass

    def test_printer_setup(self):
        pass

    def test_module_setup(self):
        pass
