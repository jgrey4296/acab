import unittest
import logging
from test_context import py_rule
from py_rule.modules.typing.type_checker import TypeChecker
from py_rule.modules.typing.type_definition import TypeDefinition
from py_rule.modules.typing.type_instance import TypeInstance
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.nodes.trie_node import TrieNode
import py_rule.error.type_exceptions as te
from py_rule.modules.typing import util as TU

class TypingTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        #hotload values
        return

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
        sen[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen)
        self.assertEqual(len(tc._variables), 1)
        #add another sentence with equivalent, but untyped, var
        sen2 = Sentence([TrieNode(x) for x in ['a','c']])
        sen2[-1]._data[TU.BIND_S] = True
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
        sen[-1]._data[TU.BIND_S] = True
        sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("a", ["a"])
        tc.add_assertion(sen)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)
        sen2 = Sentence([TrieNode(x) for x in ['a','c']])
        sen2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen2)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)

    def test_basic_query(self):
        """ ::a END, a.$b """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen1)

        query_sen = Sentence([TrieNode(x) for x in ["a","b"]])
        query_sen[-1]._data[TU.BIND_S] = True
        results = tc.query(query_sen)
        self.assertEqual(len(results), 1)

    def test_basic_query_fail(self):
        """ ::a END, a.$b """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen1)

        query_sen = Sentence([TrieNode(x) for x in ["a","c"]])
        query_sen[-1]._data[TU.BIND_S] = True
        results = tc.query(query_sen)
        self.assertEqual(len(results), 0)

    def test_basic_inference(self):
        """ ::a END, a.$b(::a), a.$c """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance("a", ["a"])
        tc.add_assertion(sen1)
        sen2 = Sentence([TrieNode(x) for x in ["a","c"]])
        sen2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen2)

        query_sen = Sentence([TrieNode(x) for x in ["a","c"]])
        query_sen[-1]._data[TU.BIND_S] = True
        query_result = tc.query(query_sen)[0]
        self.assertIsNone(query_result._type)

        tc.validate()
        query_result = tc.query(query_sen)[0]
        self.assertIsNotNone(query_result._type)
        self.assertEqual(query_result._type, sen1[-1]._data[TU.TYPE_DEC_S])

    def test_type_conflict(self):
        """ ::a END, a.$b(::a), a.$c(::b) """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("a", ["a"], [], []))
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance("a", ["a"])
        tc.add_assertion(sen1)
        sen2 = Sentence([TrieNode(x) for x in ["a","c"]])
        sen2[-1]._data[TU.BIND_S] = True
        sen2[-1]._data[TU.TYPE_DEC_S] = TypeInstance("b", ["b"])
        tc.add_assertion(sen2)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()

    def test_type_undefined(self):
        """ a.$b(::a) """
        tc = TypeChecker()
        sen1 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance("a", ["a"])
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
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance("String", ["String"])
        tc.add_assertion(sen1)
        sen2 = Sentence([TrieNode(x) for x in ["a","b"]])
        sen2[-1]._data[TU.BIND_S] = True
        sen2[-1]._data[TU.TYPE_DEC_S] = TypeInstance("Number", ["Number"])
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
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("String", ["String"])
        tc.add_definition(TypeDefinition("first", ["first"], [struct_sen], []))

        sen = Sentence([TrieNode(x) for x in ["a", "b"]])
        sen[0]._data[TU.TYPE_DEC_S] = TypeInstance("first", ["first"])

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
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("String", ["String"])
        tc.add_definition(TypeDefinition("first", ["first"], [struct_sen], []))

        sen = Sentence([TrieNode(x) for x in ["a", "name", "y"]])
        sen[0]._data[TU.TYPE_DEC_S] = TypeInstance("first", ["first"])
        sen[-1]._data[TU.BIND_S] = True
        sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("Number", ["Number"])

        tc.add_assertion(sen)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()

    def test_typing_nested_vars(self):
        """ ::String: END, ::Number: END
        ::a.test.type: name.$x(::String).$y(::Number) END
        .bob(::.a.test.type).name.$z.$q
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        struct_sen = Sentence([TrieNode(x) for x in ["name", "x", "y"]])
        struct_sen[-2]._data[TU.BIND_S] = True
        struct_sen[-2]._data[TU.TYPE_DEC_S] = TypeInstance("String", ["String"])
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("Number", ["Number"])
        tc.add_definition(TypeDefinition("first", ["first", "type"], [struct_sen], []))

        sen = Sentence([TrieNode(x) for x in ["bob", "name", "z", "q"]])
        sen[0]._data[TU.TYPE_DEC_S] = TypeInstance("first", ["first", "type"])
        sen[-2]._data[TU.BIND_S] = True
        sen[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen)

        #Query the First Variable, should be untyped
        query_sen = Sentence([TrieNode(x) for x in ["bob","name","z"]])
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertIsNone(tc.query(query_sen)[0]._type)
        #then the second, should be untyped
        query_sen = Sentence([TrieNode(x) for x in ["bob","name","z", "q"]])
        query_sen[-1]._data[TU.BIND_S] = True
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertIsNone(tc.query(query_sen)[0]._type)

        tc.validate()
        #Check the first var is inferred
        query_sen = Sentence([TrieNode(x) for x in ["bob","name","z"]])
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type, TypeInstance("String", ["String"]))
        #Check the second var is inferred
        query_sen = Sentence([TrieNode(x) for x in ["bob","name","z", "q"]])
        query_sen[-2]._data[TU.BIND_S] = True
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type, TypeInstance("Number", ["Number"]))

    def test_typing_nested_types(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        #Small Type
        type_1_sen = Sentence([TrieNode(x) for x in ["name", "x"]])
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_1_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("String", ["String"])
        tc.add_definition(TypeDefinition("smallType", ["small", "type"], [type_1_sen], []))

        #Large Type
        type_2_sen = Sentence([TrieNode(x) for x in ["component", "x"]])
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("smallType", ["small", "type"])
        tc.add_definition(TypeDefinition("largeType", ["large", "type"], [type_2_sen], []))

        assertion = Sentence([TrieNode(x) for x in ["a", "component", "q", "name", "w"]])
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance("largeType", ["large", "type"])
        assertion[2]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.BIND_S] = True

        tc.add_assertion(assertion)

        query_sen1 = Sentence([TrieNode(x) for x in ["a","component","q"]])
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = Sentence([TrieNode(x) for x in ["a", "component", "q", "name", "w"]])
        query_sen2[-3]._data[TU.BIND_S] = True
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertIsNone(tc.query(query_sen2)[0]._type)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance("smallType", ["small", "type"]))
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance("String", ["String"]))

    def test_typing_nested_types_fail(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w(::Number)
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        #Small Type
        type_1_sen = Sentence([TrieNode(x) for x in ["name", "x"]])
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_1_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("String", ["String"])
        tc.add_definition(TypeDefinition("smallType", ["small", "type"], [type_1_sen], []))

        #Large Type
        type_2_sen = Sentence([TrieNode(x) for x in ["component", "x"]])
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("smallType", ["small", "type"])
        tc.add_definition(TypeDefinition("largeType", ["large", "type"], [type_2_sen], []))

        assertion = Sentence([TrieNode(x) for x in ["a", "component", "q", "name", "w"]])
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance("largeType", ["large", "type"])
        assertion[2]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.TYPE_DEC_S] = TypeInstance("Number", ["Number"])
        tc.add_assertion(assertion)

        query_sen1 = Sentence([TrieNode(x) for x in ["a","component","q"]])
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = Sentence([TrieNode(x) for x in ["a", "component", "q", "name", "w"]])
        query_sen2[-3]._data[TU.BIND_S] = True
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance("Number", ["Number"]))

        with self.assertRaises(te.TypeConflictException):
            tc.validate()


    def test_typing_polytype(self):
        """ ::String: END, ::Number: END
        ::polytype[$x]: name.$x END
        a(::polytype(::String)).name.$q
        b(::polytype(::Number)).name.$t
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        #polytype
        type_1_sen = Sentence([TrieNode(x) for x in ["name", "x"]])
        type_1_sen[-1]._data[TU.BIND_S] = True
        param = TrieNode("x")
        param._data[TU.BIND_S] = True
        tc.add_definition(TypeDefinition("polyType", ["polyType"], [type_1_sen], [param]))

        #assertions
        assertion = Sentence([TrieNode(x) for x in ["a", "name", "q"]])
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance("polyType", ["polyType"], [TypeInstance("String", ["String"])])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        assertion2 = Sentence([TrieNode(x) for x in ["b", "name", "t"]])
        assertion2[0]._data[TU.TYPE_DEC_S] = TypeInstance("polyType", ["polyType"], [TypeInstance("Number", ["Number"])])
        assertion2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion2)

        #queries
        query_sen1 = Sentence([TrieNode(x) for x in ["a","name","q"]])
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = Sentence([TrieNode(x) for x in ["b","name","t"]])
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertIsNone(tc.query(query_sen2)[0]._type)

        tc.validate()
        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance("String", ["String"]))
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance("Number", ["Number"]))

    def test_typing_polytype_nested(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x]: name.$x END
        ::ptypeTwo[$y]: nested(::ptypeOne(::$y)) END
        a(::ptypeTwo(::String)).nested.name.$x
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        #polytype 1
        type_1_sen = Sentence([TrieNode(x) for x in ["name", "x"]])
        type_1_sen[-1]._data[TU.BIND_S] = True
        param = TrieNode("x")
        param._data[TU.BIND_S] = True
        tc.add_definition(TypeDefinition("polyTypeOne", ["polyTypeOne"], [type_1_sen], [param]))

        #polytype 2
        type_2_sen = Sentence([TrieNode(x) for x in ["nested"]])
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_param = TrieNode("y")
        type_2_param._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance("polyTypeOne",
                                                             ["polyTypeOne"],
                                                             [type_2_param])
        param2 = TrieNode("y")
        param2._data[TU.BIND_S] = True
        tc.add_definition(TypeDefinition("polyTypeTwo",
                                         ["polyTypeTwo"],
                                         [type_2_sen],
                                         [param2]))

        #Assertion
        assertion = Sentence([TrieNode(x) for x in ["a", "nested", "name", "x"]])
        assertion[0]._data[TU.BIND_S] = True
        assertion_param = TypeInstance("String", ["String"])
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance("polyTypeTwo",
                                                           ["polyTypeTwo"],
                                                           [assertion_param])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        #queries
        query_sen1 = Sentence([TrieNode(x) for x in ["a","nested", "name","x"]])
        query_sen1[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance("String", ["String"]))

    def test_typing_polytype_multi_param(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x, $y]: place.$x, age.$y END
        a(::ptypeOne(::String, ::Number)).place.$q
        a.age.$w
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition("String", ["String"], [], []))
        tc.add_definition(TypeDefinition("Number", ["Number"], [], []))

        #polytype
        type_1_sen = Sentence([TrieNode(x) for x in ["name", "x"]])
        type_1_sen[-1]._data[TU.BIND_S] = True
        param = TrieNode("x")
        param._data[TU.BIND_S] = True
        type_2_sen = Sentence([TrieNode(x) for x in ["age", "y"]])
        type_2_sen[-1]._data[TU.BIND_S] = True
        param2 = TrieNode("y")
        param2._data[TU.BIND_S] = True
        tc.add_definition(TypeDefinition("polyType",
                                         ["polyType"],
                                         [type_1_sen, type_2_sen],
                                         [param, param2]))

        #assertions
        assertion = Sentence([TrieNode(x) for x in ["a", "name", "q"]])
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance("polyType",
                                                           ["polyType"],
                                                           [TypeInstance("String", ["String"]),
                                                            TypeInstance("Number", ["Number"])])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        assertion2 = Sentence([TrieNode(x) for x in ["a", "age", "w"]])
        assertion2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion2)

        #queries
        query_sen1 = Sentence([TrieNode(x) for x in ["a","name","q"]])
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = Sentence([TrieNode(x) for x in ["a","age","w"]])
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertIsNone(tc.query(query_sen2)[0]._type)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance("String", ["String"]))
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance("Number", ["Number"]))


    def test_typing_context_clear(self):
        tc = TypeChecker()

        sen = Sentence([TrieNode(x) for x in ["a", "test", "var"]])
        sen[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen)

        sen2 = Sentence([TrieNode(x) for x in ["var", "blah"]])
        sen2[0]._data[TU.BIND_S] = True
        tc.add_assertion(sen2)

        self.assertEqual(len(tc._variables), 1)
        self.assertIsNotNone(tc.query(sen)[0]._var_node)
        self.assertEqual(len(tc.query(sen2)), 1)

        tc.clear_context()

        self.assertEqual(len(tc._variables), 0)
        self.assertIsNone(tc.query(sen)[0]._var_node)
        self.assertEqual(len(tc.query(sen2)), 0)

    def test_typing_polytype_fail(self):
        # TODO
        return

    def test_typing_polytype_nested_fail(self):
        # TODO
        return

    def test_typing_polytype_multi_param_fail(self):
        # TODO
        return

    def test_polytype_lacking_param(self):
        # TODO
        return

    def test_polytype_nested_lacking_param(self):
        # TODO
        return

    def test_add_rule(self):
        # TODO
	    return

    def test_add_operation(self):
        # TODO
        return

    def test_infer_from_operation(self):
        # TODO
        return

    def test_infer_polytype_param_from_use(self):
        # TODO
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
