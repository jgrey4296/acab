import unittest
import IPython
import logging
from test_context import py_rule
from py_rule.typing.type_checker import TypeChecker
from py_rule.typing.ex_types import TypeDefinition, MonoTypeVar
from py_rule.abstract.sentence import Sentence
from py_rule.trie.nodes.trie_node import TrieNode
import py_rule.utils as utils

class TypingTests(unittest.TestCase):

    def setUp(self):
	    return 1

    def tearDown(self):
	    return 1

    #----------
    def test_init(self):
        tc = TypeChecker()
        self.assertIsNotNone(tc)
        self.assertIsNotNone(tc._definitions)
        self.assertIsNotNone(tc._declarations)
        self.assertIsNotNone(tc._variables)
        self.assertIsNotNone(tc._context_prefix_stack)

    def test_add_definition(self):
        tc = TypeChecker()
        self.assertEqual(len(tc._definitions), 0)
        tc.add_definition(TypeDefinition("test",
                                         ["a"],
                                         [],
                                         []))
        self.assertEqual(len(tc._definitions), 1)

    def test_add_assertion(self):
        tc = TypeChecker()
        self.assertEqual(len(tc._declarations), 0)
        tc.add_assertion(Sentence([TrieNode(x) for x in ['a','b','c','d']]))
        self.assertEqual(len(tc._declarations), 4)

    def test_merge_equivalent_variables(self):
        tc = TypeChecker()
        #add a sentence with a typed variable
        sen = Sentence([TrieNode(x) for x in ['a','b']])
        sen[-1]._data[utils.BIND_S] = True
        sen[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("test", ["a"])
        tc.add_assertion(sen)
        self.assertEqual(len(tc._variables), 1)
        #add another sentence with equivalent, but untyped, var
        sen2 = Sentence([TrieNode(x) for x in ['a','c']])
        sen2[-1]._data[utils.BIND_S] = True
        tc.add_assertion(sen2)
        self.assertEqual(len(tc._variables), 2)
        #merge
        tc._merge_equivalent_nodes()
        self.assertEqual(len(tc._variables), 1)
        self.assertEqual({str(x) for x in tc._variables.get_nodes()[0]._nodes}, {'$b','$c'})

    def test_get_known_typed_nodes(self):
        tc = TypeChecker()
        self.assertFalse(tc._get_known_typed_nodes())
        sen = Sentence([TrieNode(x) for x in ['a','b']])
        sen[-1]._data[utils.BIND_S] = True
        sen[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("test", ["a"])
        tc.add_assertion(sen)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)
        sen2 = Sentence([TrieNode(x) for x in ['a','c']])
        sen2[-1]._data[utils.BIND_S] = True
        tc.add_assertion(sen2)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)

    def test_add_rule(self):
	    return

    def test_basic_component_inference(self):
	    return

    def test_type_conflict(self):
	    return

    def test_type_undefined(self):
	    return

    def test_type_redefinition(self):
	    return

    def test_variable_conflict(self):
	    return

    def test_structure_mismatch(self):
	    return

    def test_structure_type_conflict(self):
	    return

    def test_typing_nested_vars(self):
	    return

    def test_typing_nested_types(self):
	    return

    def test_typing_nested_types_fail(self):
	    return

    def test_typing_polytype(self):
	    return

    def test_typing_polytype_nested(self):
	    return

    def test_typing_polytype_nested_fail(self):
	    return




if __name__ == "__main__":
	#use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.typing_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
