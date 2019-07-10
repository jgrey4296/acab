import unittest
import IPython
import logging
from test_context import py_rule
from py_rule.typing.type_checker import TypeChecker
from py_rule.typing.ex_types import TypeDefinition, MonoTypeVar
from py_rule.abstract.sentence import Sentence
from py_rule.trie.nodes.trie_node import TrieNode
import py_rule.typing.type_exceptions as te
import py_rule.utils as utils

class TypingTests(unittest.TestCase):

    def setUp(self):
	    return 1

    def tearDown(self):
	    return 1

    #----------
    def test_init(self):
        """ Basic verification of expected structure """
        tc = TypeChecker()
        self.assertIsNotNone(tc)
        self.assertIsNotNone(tc._definitions)
        self.assertIsNotNone(tc._declarations)
        self.assertIsNotNone(tc._variables)

    def test_add_definition(self):
        """ :: a END """
        tc = TypeChecker()
        self.assertEqual(len(tc._definitions), 0)
        tc.add_definition(TypeDefinition("a",
                                         ["a"],
                                         [],
                                         []))
        self.assertEqual(len(tc._definitions), 1)

    def test_add_assertion(self):
        """ a.b.c.d """
        tc = TypeChecker()
        self.assertEqual(len(tc._declarations), 0)
        tc.add_assertion(Sentence([TrieNode(x) for x in ['a','b','c','d']]))
        self.assertEqual(len(tc._declarations), 4)

    def test_merge_equivalent_variables(self):
        """ a.$b, a.$c """
        tc = TypeChecker()
        #add a sentence with a typed variable
        sen = Sentence([TrieNode(x) for x in ['a','b']])
        sen[-1]._data[utils.BIND_S] = True
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
        """ a.$b(::a), a.$c """
        tc = TypeChecker()
        self.assertFalse(tc._get_known_typed_nodes())
        sen = Sentence([TrieNode(x) for x in ['a','b']])
        sen[-1]._data[utils.BIND_S] = True
        sen[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("a", ["a"])
        tc.add_assertion(sen)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)
        sen2 = Sentence([TrieNode(x) for x in ['a','c']])
        sen2[-1]._data[utils.BIND_S] = True
        tc.add_assertion(sen2)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)

    def test_basic_query(self):
        """ ::a END, a.$b """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[utils.BIND_S] = True
        tc.add_assertion(sen1)

        query_sen = Sentence([TrieNode(x) for x in ["a","b"]])
        query_sen[-1]._data[utils.BIND_S] = True
        results = tc.query(query_sen)
        self.assertEqual(len(results), 1)

    def test_basic_query_fail(self):
        """ ::a END, a.$b """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[utils.BIND_S] = True
        tc.add_assertion(sen1)

        query_sen = Sentence([TrieNode(x) for x in ["a","c"]])
        query_sen[-1]._data[utils.BIND_S] = True
        results = tc.query(query_sen)
        self.assertEqual(len(results), 0)

    def test_basic_inference(self):
        """ ::a END, a.$b(::a), a.$c """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[utils.BIND_S] = True
        sen1[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("a", ["a"])
        tc.add_assertion(sen1)
        sen2 = Sentence([TrieNode(x) for x in ["a","c"]])
        sen2[-1]._data[utils.BIND_S] = True
        tc.add_assertion(sen2)

        query_sen = Sentence([TrieNode(x) for x in ["a","c"]])
        query_sen[-1]._data[utils.BIND_S] = True
        query_result = tc.query(query_sen)[0]
        self.assertIsNone(query_result._type)

        tc.validate()
        query_result = tc.query(query_sen)[0]
        self.assertIsNotNone(query_result._type)
        self.assertEqual(query_result._type, sen1[-1]._data[utils.TYPE_DEC_S])


    def test_type_conflict(self):
        """ ::a END, a.$b(::a), a.$c(::b) """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[utils.BIND_S] = True
        sen1[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("a", ["a"])
        tc.add_assertion(sen1)
        sen2 = Sentence([TrieNode(x) for x in ["a","c"]])
        sen2[-1]._data[utils.BIND_S] = True
        sen2[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("b", ["b"])
        tc.add_assertion(sen2)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()

    def test_type_undefined(self):
        """ a.$b(::a) """
        tc = TypeChecker()
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[utils.BIND_S] = True
        sen1[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("a", ["a"])
        tc.add_assertion(sen1)

        with self.assertRaises(te.TypeUndefinedException):
            tc.validate()

    def test_type_redefinition(self):
        """ ::a END, ::a END """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        with self.assertRaises(te.TypeRedefinitionException):
            tc.add_definition(TypeDefinition("a", ["a"], [], []))

    def test_variable_conflict(self):
        """ ::String: END ::Number: END
        a.$x(::.String) b.$x(::.Number)
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[utils.BIND_S] = True
        sen1[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("String", ["String"])
        tc.add_assertion(sen1)
        sen2 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen2[-1]._data[utils.BIND_S] = True
        sen2[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("Number", ["Number"])
        with self.assertRaises(te.TypeConflictException):
            tc.add_assertion(sen2)


    def test_structure_mismatch(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::.String) END
        a(::first).b
        """
        tc =TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        struct_sen = Sentence([TrieNode(x) for x in ["name", "x"]])
        struct_sen[-1]._data[utils.BIND_S] = True
        struct_sen[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("String", ["String"])
        tc.add_definition(TypeDefinition("first", ["first"], [struct_sen], []))

        sen = Sentence([TrieNode(x) for x in ["a", "b"]])
        sen[0]._data[utils.TYPE_DEC_S] = MonoTypeVar("first", ["first"])

        tc.add_assertion(sen)

        with self.assertRaises(te.TypeStructureMismatch):
            tc.validate()


    def test_structure_type_conflict(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::String) END
        a(::first).name.$y(::Number)
        """
        tc =TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        struct_sen = Sentence([TrieNode(x) for x in ["name", "x"]])
        struct_sen[-1]._data[utils.BIND_S] = True
        struct_sen[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("String", ["String"])
        tc.add_definition(TypeDefinition("first", ["first"], [struct_sen], []))

        sen = Sentence([TrieNode(x) for x in ["a", "name", "y"]])
        sen[0]._data[utils.TYPE_DEC_S] = MonoTypeVar("first", ["first"])
        sen[-1]._data[utils.BIND_S] = True
        sen[-1]._data[utils.TYPE_DEC_S] = MonoTypeVar("Number", ["Number"])

        tc.add_assertion(sen)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()

    def test_typing_nested_vars(self):
        """ ::String: END, ::Number: END
        ::a.test.type: name.$x(::String).$y(::Number) END
        .bob(::.a.test.type).name.$z.$q
        """
        return

    def test_typing_nested_types(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w
        """
        return

    def test_typing_nested_types_fail(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w
        """
        return

    def test_typing_polytype(self):
        """ ::String: END, ::Number: END
        ::polytype[$x]: name.$x END
        a(::polytype(::String)).name.$q
        b(::polytype(::Number)).name.$t
        """
        return

    def test_typing_polytype_nested(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x]: place.$x END
        ::ptypeTwo[$y]: nested(::ptypeOne(::$y)) END
        a(::ptypeTwo(::String)).nested.place.$x
        """
        return

    def test_typing_polytype_nested_fail(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x, $y]: place.$x, age.$y END
        a(::ptypeOne(::String, ::Number)).place.$x
        a.age.$x
        """
        return

    def test_add_rule(self):
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
