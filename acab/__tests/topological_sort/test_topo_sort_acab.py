#!/usr/bin/env python3

#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging

from acab.abstract.config.config import AcabConfig
AcabConfig.Get().read("acab/abstract/config")

from acab.abstract.core.values import AcabValue, AcabStatement, Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.core.contexts import Contexts

class TopologicalOrderedAcabTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        root_logger.getLogger('').setLevel(root_logger.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = root_logger.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(root_logger.DEBUG)

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.WARNING)

        logging = root_logger.getLogger(__name__)
        logging.setLevel(root_logger.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    # use testcase snippet
    # mock.Mock / MagicMock
    # create_autospec
    # @patch(' ') / with patch.object(...)


    # Configuration
    def test_config_sections(self):
        config = AcabConfig.Get()
        self.assertIsInstance(config, AcabConfig)
        config2 = AcabConfig.Get()
        self.assertIs(config, config2)

    def test_config_value(self):
        config = AcabConfig.Get()
        value = config.value("Data", "ROOT")
        self.assertEqual(value, "__root")

    def test_config_prepare(self):
        config = AcabConfig.Get()
        prep_tuple = config.prepare("Data", "ROOT")
        self.assertIsInstance(prep_tuple, tuple)
        self.assertEqual(len(prep_tuple), 5)

    def test_modal_spec(self):
        config = AcabConfig.Get()
        self.assertTrue(config.modal_enums)
        self.assertTrue(config.modal_defaults)
        self.assertTrue(config.modal_printing)
        self.assertTrue(config.modal_syntax_lookup)
        # TODO Check values *in* the modal structures

    def test_config_value_missing(self):
        pass

    def test_config_prepare_missing(self):
        pass

    def test_modal_spec_missing(self):
        pass

    # -> ClosedSet[Values, Node]
    # Creation,
    def test_value_create(self):

        pass

    def test_sentence_creation(self):
        pass

    def test_node_creation(self):
        pass

    def test_context_creation(self):
        pass

    # node root, containment, core add, get, remove, has etc
    def test_value_equality(self):
        pass

    def test_value_copy(self):
        pass

    def test_value_bind(self):
        pass

    def test_sentence_containment(self):
        pass

    def test_sentence_bind(self):
        pass

    def test_sentence_attach_statement(self):
        pass

    def test_node_root(self):
        pass

    def test_node_has(self):
        pass

    def test_node_add(self):
        pass

    def test_node_get(self):
        pass

    def test_node_has(self):
        pass


    # -> Abstractions[Structures, Rule]
    # creation
    def test_structure_creation(self):
        pass

    def test_operator_constructor(self):
        pass

    def test_component_constructor(self):
        pass

    def test_container_constructor(self):
        pass

    def test_structure_constructor(self):
        pass

    def test_operator_bind(self):
        pass

    def test_component_bind(self):
        pass

    def test_container_bind(self):
        pass

    def test_structure_bind(self):
        pass

    # -> Semantics[ClosedSet, Abstractions]
    def test_node_semantics(self):
        pass

    def test_context_semantics(self):
        pass

    def test_structure_semantics(self):
        pass

    def test_struct_semantics_retrieval(self):
        pass

    def test_basic_node_semantics(self):
        pass

    # Trie *Implementation*
    def test_trie(self):
        # construction
        pass

    # -> Bootstrap, Working Memory, Engine
    # uses trie
    def test_bootstrap_constructor(self):
        pass

    def test_working_memory_creation(self):
        pass

    def test_engine_constructor(self):
        pass

    def test_bootstrap_add(self):
        pass

    def test_bootstrap_query(self):
        pass

    def test_wm_construct_parsers(self):
        pass

    def test_wm_interrupt(self):
        pass

    def test_engine_loading(self):
        pass

    def test_engine_operator_retrieval(self):
        pass

    # -> Reduction
    # to_sentences of everything
    def test_to_sentences(self):
        pass

    # -> Core Input/Output[Actions, Printing, Parsing]
    def test_parse_utils(self):
        pass

    def test_print_utils(self):
        pass

    def test_basic_output_print(self):
        pass

    def test_trie_wm_parsing(self):
        pass

    # assembled semantics

    # -> Modules[Values, Parsers, Semantics]
    # Typing
    # Repl
    # Time
    # Numbers
    # Operators
