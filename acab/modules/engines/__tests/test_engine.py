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

import acab.modules.printing.printers as Printers
import acab.modules.semantics.abstractions as ASem
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.values import Sentence
from acab.abstract.interfaces.engine_interface import AcabEngine_Interface
from acab.abstract.interfaces.printing_interfaces import PrintSystem
from acab.modules.engines.basic_engine import AcabBasicEngine
from acab.modules.operators import query as QOP
from acab.modules.parsing.exlo.el_dsl import EL_Parser
import acab.modules.parsing.exlo.parsers.FactParser as FP
from acab.modules.parsing.exlo.parsers import QueryParser as QP
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)
from acab.modules.semantics.system import BasicSemanticSystem

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])


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
        printer   = create_autospec(PrintSystem)
        basic     = AcabBasicEngine(parser=parser,
                                    semantics=semantics,
                                    printer=printer,
                                    modules=[])

        self.assertIsInstance(basic, AcabEngine_Interface)
        self.assertTrue(basic.initialised)

        parser.assert_parsers.assert_called_once()
        parser.query_parsers.assert_called_once()

    def test_parser_setup(self):
        parser    = EL_Parser()
        # Build the default semantics
        node_sem    = BasicNodeSemantics("_:node")
        trie_sem    = BreadthTrieSemantics("_:trie", default=(node_sem, None),
                                           handlers=[], structs=[])

        query_sem   = ASem.QueryAbstraction(QUERY_SEM_HINT)
        action_sem  = ASem.ActionAbstraction(ACTION_SEM_HINT)
        rule_sem    = ASem.AtomicRuleAbstraction(RULE_SEM_HINT)
        trans_sem   = ASem.TransformAbstraction(TRANSFORM_SEM_HINT)
        cont_sem    = ASem.ContainerAbstraction("_:CONTAINER")

        trie_struct = BasicNodeStruct.build_default("_:trie")
        semantics   = BasicSemanticSystem(handlers=[cont_sem,
                                                    query_sem,
                                                    action_sem,
                                                    rule_sem,
                                                    trans_sem,
                                                    trie_sem],
                                          structs=[trie_struct],
                                          default=(trie_sem, trie_struct))

        # Build the default printer
        printer    = PrintSystem(handlers=[Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                           Printers.ConstraintAwareValuePrinter("_:ATOM"),
                                           Printers.ProductionComponentPrinter("_:COMPONENT"),
                                           Printers.ExplicitContainerPrinter("_:CONTAINER"),
                                           Printers.ImplicitContainerPrinter("_:IMPLICIT_CONTAINER"),
                                           Printers.StructurePrinter("_:STRUCTURE"),
                                           Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                           Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL"),
                                           ],
                                 structs=[],
                                 settings={"MODAL": "exop"})

        # Create the engine
        basic     = AcabBasicEngine(parser=parser,
                                    semantics=semantics,
                                    printer=printer,
                                    modules=[])

        query = basic.query("a.test.sentence?")
        self.assertFalse(bool(query))
        basic.insert("a.test.sentence")
        result = basic.query("a.test.sentence?")
        self.assertTrue(bool(result))

    def test_printer_setup(self):



        pass

    def test_module_setup(self):
        pass
