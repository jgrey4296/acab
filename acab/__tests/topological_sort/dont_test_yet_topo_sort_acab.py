#!/usr/bin/env python3

#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as logmod
import unittest
import unittest.mock as mock
from os.path import split, splitext

import acab

config = acab.setup()

from acab.core.config.config import ConfigSpec
from acab.core.data import instruction as PA
from acab.core.data.acab_struct import AcabStruct
from acab.core.data.instruction import Instruction
from acab.core.data.node import AcabNode
from acab.core.data.sentence import Sentence
from acab.core.data.value import AcabValue
from acab.interfaces.data import Structure_i
from acab.interfaces.semantic import ValueSemantics
from acab.modules.operators.query.query_operators import EQ
from acab.modules.semantics.basic_node_semantics import BasicNodeSemantics
from acab.modules.structures.trie.trie import Trie

OPERATOR_TYPE_PRIM_S  = config.prepare("Type.Primitive", "OPERATOR")()
CONTAINER_TYPE_PRIM_S = config.prepare("Type.Primitive", "CONTAINER")()
COMPONENT_TYPE_PRIM_S = config.prepare("Type.Primitive", "COMPONENT")()
ANON_VALUE            = config.prepare("Symbols", "ANON_VALUE")()

# TODO duplicate this, but for failures
class TopologicalOrderedAcabTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        logmod.getLogger('').setLevel(logmod.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = logmod.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(logmod.DEBUG)

        console = logmod.StreamHandler()
        console.setLevel(logmod.WARNING)

        logging = logmod.getLogger(__name__)
        logging.setLevel(logmod.DEBUG)
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
# Config
    def test_config_singleton(self):
        """ Check the config obj is a singleton"""
        config = AcabConfig()
        self.assertIsInstance(config, AcabConfig)
        config2 = AcabConfig()
        self.assertIs(config, config2)

    def test_config_prepare(self):
        """
        Check values can be retrieved
        """
        config = AcabConfig()
        value = config.prepare("Data", "ROOT")()
        self.assertEqual(value, "__root")

    def test_config_prepare(self):
        """ Check values can be prepared """
        config = AcabConfig()
        prep_tuple = config.prepare("Data", "ROOT")
        self.assertIsInstance(prep_tuple, ConfigSpec)

    def test_modal_spec(self):
        """ Check modal fields exist """
        config = AcabConfig()
        # TODO update these
        self.assertTrue(config.enums)
        self.assertTrue(config.defaults)
        self.assertTrue(config.printing_extension)
        self.assertTrue(config.syntax_extension)
        # TODO Check values *in* the modal structures

    def test_config_prepare_missing(self):
        """ Check error is thrown for missing value """
        config = AcabConfig()
        with self.assertRaises(Exception):
            config.prepare("blah", "bloo")

    def test_config_prepare_missing(self):
        """ Check config errors if you prepare
        a missing value """
        config = AcabConfig()
        with self.assertRaises(Exception):
            config.prepare("blah", "bloo")

    def test_modal_spec_missing(self):
        """
        Check config errors when you try to use missing modal values
        """
        config = AcabConfig()
        with self.assertRaises(Exception):
            config.modal_enums['blah']

# Data

    # -> ClosedSet[Values, Node]
    # Creation,
    def test_value_create(self):
        """ Check AcabValues construct well"""
        value = AcabValue("test")
        self.assertIsInstance(value, AcabValue)
        self.assertEqual(value.name, "test")
        self.assertEqual(value.value, "test")
        self.assertEqual(value.type, Sentence.build([config.prepare("Data", "TYPE_BASE")]))


    def test_value_build(self):
        """ Check AcabValues don't nest """
        value1 = AcabValue("value")
        self.assertEqual(value1, value1)
        value2 = AcabValue.build(value1)
        self.assertEqual(value1, value2)
        self.assertFalse(isinstance(value2.value, AcabValue))

    def test_value_build_2(self):
        """ Check AcabValues don't nest """
        value1 = "test"
        value2 = AcabValue.build(value1)
        self.assertIsInstance(value1, str)
        self.assertIsInstance(value2, AcabValue)
    def test_sentence_creation(self):
        """ Check Sentences form from multiple Values """
        basic_sentence = Sentence.build(["a", "test", "sentence"])
        self.assertIsInstance(basic_sentence, Sentence)
        self.assertEqual(len(basic_sentence), 3)
        words = basic_sentence.words
        self.assertEqual(len(words), 3)
        self.assertTrue(all([isinstance(x, AcabValue) for x in words]))
        self.assertEqual("test", basic_sentence[1])
    def test_node_creation(self):
        """ Check nodes construct to contain values """
        value = AcabValue("test")
        node = AcabNode(value)
        self.assertEqual(node.value, value)
        self.assertFalse(node.children)


    # node root, containment, core add, get, remove, has etc
    def test_value_equality(self):
        """ Check two values can equal each other in simple cases """
        value1 = AcabValue("test")
        value2 = AcabValue("test")
        self.assertEqual(value1, value2)

    def test_value_inequality(self):
        value1 = AcabValue("test")
        value2 = AcabValue("blah")
        self.assertNotEqual(value1, value2)


    def test_value_copy(self):
        """ Check a value can be copied, and is independent """
        value1 = AcabValue("test")
        valueCopy = value1.copy()
        self.assertEqual(value1, valueCopy)

    def test_value_bind(self):
        """ Check a value binds appropriately """
        bind_data = {"test" : "blah"}
        value = AcabValue("test", data={config.prepare("Value.Structure", "BIND"): True})
        bind_result = value.bind(bind_data)
        self.assertIsInstance(bind_result, AcabValue)
        self.assertEqual(bind_result, "blah")


    def test_sentence_containment(self):
        """ Check values can be detected in a sentence """
        sen = Sentence.build(["a", "test", "sentence"])
        value = AcabValue("test")
        self.assertTrue(value in sen)

    def test_sentence_squared(self):
        """ Check a sentence *in* a sentence """
        sen1 = Sentence.build(["a", "test", "sentence"])
        sen2 = Sentence.build(["a", "further", sen1])
        self.assertIn(sen1, sen2)

    def test_sentence_squared_fail(self):
        """ Check a sentence *in* a sentence doesn't always pass """
        sen1 = Sentence.build(["a", "test", "sentence"])
        sen2 = Sentence.build(["a", "different", "sentence"])
        sen3 = Sentence.build(["a", "further", sen1])
        self.assertNotIn(sen2, sen3)

    def test_sentence_containment_fail(self):
        sen = Sentence.build(["a", "test", "sentence"])
        value = AcabValue("blah")
        self.assertFalse(value in sen)

    def test_sentence_bind(self):
        """ Check sentences can bind sub words to values"""
        bind_data = {"test": "aweg"}
        sen = Sentence.build(["a", AcabValue("test", data={config.prepare("Value.Structure", "BIND"): True}), "sentence"])
        sen_copy = sen.bind(bind_data)
        self.assertIsInstance(sen_copy, Sentence)
        self.assertNotEqual(sen, sen_copy)

        self.assertFalse("aweg" in sen)
        self.assertTrue("aweg" in sen_copy)



    def test_sentence_attach_statement(self):
        """ Check sentences can attach statements to leaf words """
        master_sen = Sentence.build(["a", "test", "sentence"])
        to_attach = Sentence.build(["another", "blah"])
        to_attach_copy = Sentence.build(["another", "blah"])

        attached = master_sen.attach_statement(to_attach)

        self.assertNotEqual(master_sen, attached)
        self.assertNotEqual(to_attach, attached)

        self.assertEqual(len(master_sen), len(attached))
        self.assertTrue(to_attach_copy in attached)

    def test_node_root(self):
        """ Check a node constructs the most basic root """
        node = AcabNode.Root()
        self.assertIsNotNone(node)
        self.assertIsInstance(node, AcabNode)


    def test_node_has(self):
        node = AcabNode.Root()
        node2 = AcabNode(AcabValue("test"))
        node.children["test"] = node2
        self.assertTrue(node.has(node2))
        self.assertTrue(node2 in node)


    def test_node_has_fail(self):
        """ Check node child testing """
        node = AcabNode.Root()
        self.assertFalse(node.has(node))
        self.assertFalse(node in node)

    def test_node_add(self):
        """ Check adding children to nodes """
        node = AcabNode.Root()
        node2 = AcabNode(AcabValue("test"))
        node.add(node2)
        self.assertTrue(node.has(node2))
        self.assertTrue(node2 in node)

    def test_node_get(self):
        """ Check node child retrieval """
        node = AcabNode.Root()
        node2 = AcabNode(AcabValue("test"))
        node.add(node2)
        got_back = node.get(AcabValue("test"))
        self.assertEqual(node2, got_back)

    def test_node_remove(self):
        node = AcabNode.Root()
        node2 = AcabNode(AcabValue("test"))
        node.add(node2)
        self.assertTrue(node2 in node)
        node.remove(AcabValue("test"))
        self.assertFalse(node2 in node)




    # -> Abstractions[Structures, Rule]
    # creation
    def test_structure_creation(self):
        """ Check The most Basic Structure: The Trie"""
        the_trie = Trie()
        self.assertIsInstance(the_trie, Trie)
        self.assertIsInstance(the_trie, Structure_i)

# Productions
    def test_production_abstraction_operator(self):
        abstract_op = PA.ProductionOperator()
        self.assertIsInstance(abstract_op, PA.ProductionOperator)
        self.assertEqual(abstract_op.name, PA.ProductionOperator.__name__)
        self.assertEqual(abstract_op.type, Sentence.build([OPERATOR_TYPE_PRIM_S]))

    def test_real_operator(self):
        abstract_op = EQ()
        self.assertIsInstance(abstract_op, PA.ProductionOperator)
        self.assertEqual(abstract_op.name, EQ.__name__)
        self.assertEqual(abstract_op.type, Sentence.build([OPERATOR_TYPE_PRIM_S]))

    def test_production_component_construction(self):
        component = PA.ProductionComponent(value=Sentence.build(["test"]))
        self.assertIsInstance(component, PA.ProductionComponent)
        self.assertEqual(component.name, ANON_VALUE)
        self.assertEqual(component.value, Sentence.build(["test"]))
        self.assertEqual(component.type, Sentence.build([COMPONENT_TYPE_PRIM_S]))

    def test_production_component_construction_params(self):
        # TODO add sugared and rebind checks
        component = PA.ProductionComponent(value=Sentence.build(["test"]), params=[Sentence.build(["a", "b", "c"])])
        self.assertIsInstance(component, PA.ProductionComponent)
        self.assertEqual(component.name, ANON_VALUE)
        self.assertEqual(component.value, Sentence.build(["test"]))
        self.assertEqual(len(component.params), 1)
        self.assertEqual(component.params[0], Sentence.build(["a", "b", "c"]))

    def test_production_container_construction(self):
        container = PA.ProductionContainer(value=[])
        self.assertIsInstance(container, PA.ProductionContainer)
        self.assertEqual(container.name, ANON_VALUE)
        self.assertIsInstance(container.value, list)
        self.assertEqual(len(container.value), 0)
        self.assertEqual(container.type, Sentence.build([CONTAINER_TYPE_PRIM_S]))


    def test_production_structure_construction(self):
        structure = PA.ProductionStructure(structure={})
        self.assertIsInstance(structure, PA.ProductionContainer)
        self.assertEqual(structure.name, ANON_VALUE)
        self.assertIsInstance(structure.value, list)
        self.assertEqual(len(structure.value), 0)
        self.assertEqual(structure.type, Sentence.build([CONTAINER_TYPE_PRIM_S]))


    def test_production_component_bind(self):
        value = AcabValue("test", data={config.prepare("Value.Structure", "BIND"): True})
        component = PA.ProductionComponent(value=Sentence.build(["test"]), params=[value])
        bind_data = {"test" : "blah"}
        bind_result = component.bind(bind_data)
        self.assertNotEqual(component, bind_result)
        self.assertIsInstance(bind_result, PA.ProductionComponent)
        self.assertEqual(bind_result.params[0], "blah")


    def test_production_container_bind(self):
        value = AcabValue("test", data={config.prepare("Value.Structure", "BIND"): True})
        container = PA.ProductionContainer(value=[], params=[value])
        bind_data = {"test" : "blah"}
        bind_result = container.bind(bind_data)
        self.assertNotEqual(container, bind_result)
        self.assertIsInstance(bind_result, PA.ProductionContainer)
        self.assertEqual(bind_result.params[0], "blah")



    def test_production_structure_bind(self):
        value = AcabValue("test", data={config.prepare("Value.Structure", "BIND"): True})
        structure = PA.ProductionStructure(structure={}, params=[value])
        bind_data = {"test" : "blah"}
        bind_result = structure.bind(bind_data)
        self.assertNotEqual(structure, bind_result)
        self.assertIsInstance(bind_result, PA.ProductionStructure)
        self.assertEqual(bind_result.params[0], "blah")

    # -> Semantics[ClosedSet, Abstractions]
# Semantics
    def test_node_semantic_creation(self):
        """ Test basic node semantic actions """
        node_sem = BasicNodeSemantics()
        self.assertIsInstance(node_sem, ValueSemantics)


    def test_basic_node(self):
        pass

    def test_trie(self):
        pass

    def test_exclusion_trie(self):
        pass

    def test_query(self):
        pass

    def test_transform(self):
        pass

    def test_action(self):
        pass

    def test_productions(self):
        pass

    def test_printing(self):
        pass

    # -> Bootstrap, Working Memory, Engine
    # uses trie
# Engine
    def test_bootstrap_constructor(self):
        pass

    def test_working_memory_creation(self):
        pass

    def test_engine_constructor(self):
        pass

    def test_bootstrap_add(self):
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
