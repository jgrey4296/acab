import unittest
import logging
from py_rule.modules.analysis.typing.type_checker import TypeChecker
from py_rule.modules.analysis.typing.type_definition import TypeDefinition
from py_rule.modules.analysis.typing.type_instance import TypeInstance
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.nodes.trie_node import TrieNode
import py_rule.error.type_exceptions as te
from py_rule.modules.analysis.typing import util as TU
from py_rule.working_memory.trie_wm.parsing import FactParser as FP


def S(*in_string):
    return Sentence([TrieNode(x) for x in in_string])


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
        self.assertIsNotNone(tc._structural_definitions)
        self.assertIsNotNone(tc._functional_definitions)
        self.assertIsNotNone(tc._declarations)
        self.assertIsNotNone(tc._variables)

    def test_add_definition(self):
        """ :: a END """
        tc = TypeChecker()
        type_def = TypeDefinition([])
        loc = FP.parseString('a.test.definition.x')[0]
        type_def.set_name_and_vars(loc)
        self.assertEqual(len(tc._structural_definitions), 0)
        tc.add_definition(type_def)
        self.assertEqual(len(tc._structural_definitions), 4)
        defs = tc._structural_definitions.get_nodes(lambda x: isinstance(x._value._value, TypeDefinition))
        self.assertEqual(1, len(defs))

    def test_add_assertion(self):
        """ a.b.c.d """
        tc = TypeChecker()
        self.assertEqual(len(tc._declarations), 0)
        tc.add_assertion(S('a','b','c','d'))
        self.assertEqual(len(tc._declarations), 4)

    def test_get_known_typed_nodes(self):
        """ a.$b(::a), a.$c """
        tc = TypeChecker()
        self.assertFalse(tc._get_known_typed_nodes())
        sen = S("a","b")
        sen[-1]._data[TU.BIND_S] = True
        sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("a"))
        tc.add_assertion(sen)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)
        sen2 = S("a","c")
        sen2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen2)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)

    def test_basic_query(self):
        """ ::a END, a.$b """
        tc = TypeChecker()
        loc = FP.parseString('a.test.definition.$x')[0]
        definition = TypeDefinition([])
        definition.set_name_and_vars(loc)
        tc.add_definition(definition)
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen1)

        query_sen = S("a","b")
        query_sen[-1]._data[TU.BIND_S] = True
        results = tc.query(query_sen)
        self.assertEqual(len(results), 1)

    def test_basic_query_fail(self):
        """ ::a END, a.$b """
        tc = TypeChecker()
        loc = FP.parseString('a.test.definition.$x')[0]
        definition = TypeDefinition([])
        definition.set_name_and_vars(loc)
        tc.add_definition(definition)
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen1)

        query_sen = S("a","c")
        query_sen[-1]._data[TU.BIND_S] = True
        results = tc.query(query_sen)
        self.assertEqual(len(results), 0)

    def test_basic_inference(self):
        """ ::a END, a.$b(::a), a.$c """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("a")))
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("a"))
        tc.add_assertion(sen1)
        sen2 = S("a","c")
        sen2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen2)

        query_sen = S("a","c")
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
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("a")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("b")))
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("a"))
        tc.add_assertion(sen1)
        sen2 = S("a","b")
        sen2[-1]._data[TU.BIND_S] = True
        sen2[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("b"))

        with self.assertRaises(te.TypeConflictException):
            tc.add_assertion(sen2)

    def test_type_undefined(self):
        """ a.$b(::a) """
        tc = TypeChecker()
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("a"))
        tc.add_assertion(sen1)

        with self.assertRaises(te.TypeUndefinedException):
            tc.validate()

    def test_type_redefinition(self):
        """ ::a END, ::a END """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("a")))
        with self.assertRaises(te.TypeRedefinitionException):
            tc.add_definition(TypeDefinition([]).set_name_and_vars(S("a")))

    def test_variable_conflict(self):
        """ ::String: END ::Number: END
        a.$x(::.String) b.$x(::.Number)
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("String"))
        tc.add_assertion(sen1)
        sen2 = S("a","b")
        sen2[-1]._data[TU.BIND_S] = True
        sen2[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("Number"))
        with self.assertRaises(te.TypeConflictException):
            tc.add_assertion(sen2)

    def test_structure_mismatch(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::.String) END
        a(::first).b
        """
        tc = TypeChecker()
        loc = FP.parseString('String')[0]
        loc2 = FP.parseString('Number')[0]
        def1 = TypeDefinition([])
        def1.set_name_and_vars(loc)
        def2 = TypeDefinition([])
        def2.set_name_and_vars(loc2)
        tc.add_definition(def1)
        tc.add_definition(def2)

        struct_sen = S("name","x")
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("String"))

        def3 = TypeDefinition([struct_sen])
        loc3 = FP.parseString("first")[0]
        def3.set_name_and_vars(loc3)
        tc.add_definition(def3)

        sen = S("a","b")
        sen[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("first"))

        tc.add_assertion(sen)

        with self.assertRaises(te.TypeStructureMismatch):
            tc.validate()

    def test_structure_type_conflict(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::String) END
        a(::first).name.$y(::Number)
        """
        tc =TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))

        struct_sen = S("name","x")
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("String"))
        tc.add_definition(TypeDefinition([struct_sen]).set_name_and_vars(S("first")))

        sen = S("a","name","y")
        sen[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("first"))
        sen[-1]._data[TU.BIND_S] = True
        sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("Number"))

        tc.add_assertion(sen)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()

    def test_typing_nested_vars(self):
        """ ::String: END, ::Number: END
        ::first.type: name.$x(::String).$y(::Number) END
        .bob(::first.type).name.$z.$q
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))

        struct_sen = S("name","x","y")
        struct_sen[-2]._data[TU.BIND_S] = True
        struct_sen[-2]._data[TU.TYPE_DEC_S] = TypeInstance(S("String"))
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("Number"))
        tc.add_definition(TypeDefinition([struct_sen]).set_name_and_vars(S("first")))

        sen = S("bob","name","z","q")
        sen[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("first"))
        sen[-2]._data[TU.BIND_S] = True
        sen[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen)

        #Query the First Variable, should be untyped
        query_sen = S("bob","name","z")
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertIsNone(tc.query(query_sen)[0]._type)
        #then the second, should be untyped
        query_sen = S("bob","name","z","q")
        query_sen[-1]._data[TU.BIND_S] = True
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertIsNone(tc.query(query_sen)[0]._type)

        tc.validate()
        #Check the first var is inferred
        query_sen = S("bob","name","z")
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type, TypeInstance(S("String")))
        #Check the second var is inferred
        query_sen = S("bob","name","z","q")
        query_sen[-2]._data[TU.BIND_S] = True
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type, TypeInstance(S("Number")))

    def test_typing_nested_types(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))

        #Small Type
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_1_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("String"))
        tc.add_definition(TypeDefinition([type_1_sen]).set_name_and_vars(S("small.type")))

        #Large Type
        type_2_sen = S("component","x")
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("small.type"))
        tc.add_definition(TypeDefinition([type_2_sen]).set_name_and_vars(S("large.type")))

        assertion = S("a","component","q","name","w")
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("large.type"))
        assertion[2]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.BIND_S] = True

        tc.add_assertion(assertion)

        query_sen1 = S("a","component","q")
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = S("a","component","q","name","w")
        query_sen2[-3]._data[TU.BIND_S] = True
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertIsNone(tc.query(query_sen2)[0]._type)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance(S("small.type")))
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance(S("String")))

    def test_typing_nested_types_fail(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w(::Number)
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))

        #Small Type
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_1_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("String"))
        tc.add_definition(TypeDefinition([type_1_sen]).set_name_and_vars(S("small.type")))

        #Large Type
        type_2_sen = S("component","x")
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("small.type"))
        tc.add_definition(TypeDefinition([type_2_sen]).set_name_and_vars(S("large.type")))

        assertion = S("a","component","q","name","w")
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("large.type"))
        assertion[2]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("Number"))
        tc.add_assertion(assertion)

        query_sen1 = S("a","component","q")
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = S("a","component","q","name","w")
        query_sen2[-3]._data[TU.BIND_S] = True
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance(S("Number")))

        with self.assertRaises(te.TypeConflictException):
            tc.validate()


    def test_typing_polytype(self):
        """ ::String: END, ::Number: END
        ::polytype[$x]: name.$x END
        a(::polytype(::String)).name.$q
        b(::polytype(::Number)).name.$t
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))

        #polytype
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        tc.add_definition(TypeDefinition([type_1_sen]).set_name_and_vars(S("poly.type"), ["x"]))

        #assertions
        assertion = S("a","name","q")
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("poly.type"), [TypeInstance(S("String"))])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        assertion2 = S("b","name","t")
        assertion2[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("poly.type"), [TypeInstance(S("Number"), ["Number"])])
        assertion2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion2)

        #queries
        query_sen1 = S("a","name","q")
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = S("b","name","t")
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertIsNone(tc.query(query_sen2)[0]._type)

        tc.validate()
        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance(S("String")))
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance(S("Number")))

    def test_typing_polytype_nested(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x]: name.$x END
        ::ptypeTwo[$y]: nested(::ptypeOne(::$y)) END
        a(::ptypeTwo(::String)).nested.name.$x
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))

        #polytype 1
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        tc.add_definition(TypeDefinition([type_1_sen]).set_name_and_vars(S("poly.type.one"), ["x"]))

        #polytype 2
        type_2_sen = S("nested")
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.TYPE_DEC_S] = TypeInstance(S("poly.type.one"), ["y"])
        tc.add_definition(TypeDefinition([type_2_sen]).set_name_and_vars(S("poly.type.two"), ["y"]))

        #Assertion
        assertion = S("a","nested","name","x")
        assertion[0]._data[TU.BIND_S] = True
        assertion_param = TypeInstance(S("String"))
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("poly.type.two"),
                                                           [assertion_param])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        #queries
        query_sen1 = S("a","nested","name","x")
        query_sen1[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance(S("String")))

    def test_typing_polytype_multi_param(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x, $y]: place.$x, age.$y END
        a(::ptypeOne(::String, ::Number)).place.$q
        a.age.$w
        """
        tc = TypeChecker()
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("String")))
        tc.add_definition(TypeDefinition([]).set_name_and_vars(S("Number")))

        #polytype
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_2_sen = S("age","y")
        type_2_sen[-1]._data[TU.BIND_S] = True
        tc.add_definition(TypeDefinition([type_1_sen, type_2_sen]).set_name_and_vars(S("poly.type"),["x", "y"]))

        #assertions
        assertion = S("a","name","q")
        assertion[0]._data[TU.TYPE_DEC_S] = TypeInstance(S("poly.type"),
                                                         [TypeInstance(S("String")),
                                                         TypeInstance(S("Number"))])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        assertion2 = S("a","age","w")
        assertion2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion2)

        #queries
        query_sen1 = S("a","name","q")
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = S("a","age","w")
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertIsNone(tc.query(query_sen1)[0]._type)
        self.assertIsNone(tc.query(query_sen2)[0]._type)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type, TypeInstance(S("String")))
        self.assertEqual(tc.query(query_sen2)[0]._type, TypeInstance(S("Number")))


    def test_typing_context_clear(self):
        tc = TypeChecker()

        sen = S("a","test","var")
        sen[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen)

        sen2 = S("var","blah")
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
