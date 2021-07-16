#!/opts/anaconda3/envs/ENV/python
import unittest
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from unittest import mock

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
        parser    = EL_Parser()
        semantics = BasicSemanticSystem(handlers=[], structs=[])
        printer   = PrintSystem(handlers=[], structs=[])
        basic = AcabBasicEngine(parser=parser,
                                semantics=semantics,
                                printer=printer)
        self.assertIsInstance(basic, AcabEngine_Interface)

        # TODO assert load_modules is called,
        # TODO assert build_DSL is called

    def test_parser_setup(self):
        pass

    def test_printer_setup(self):
        pass

    def test_module_setup(self):
        pass
