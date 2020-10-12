import unittest
import logging

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.type_base import TypeInstance, ATOM
from acab.abstract.core.value import AcabValue
from acab.abstract.data.node import AcabNode

from acab.abstract.engine.bootstrap_parser import BootstrapParser

from acab.abstract.rule import action
from acab.abstract.rule.production_operator import ProductionOperator
from acab.abstract.rule.transform import TransformComponent

from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing import util as TU
from acab.modules.analysis.typing.type_checker import TypeChecker
from acab.modules.analysis.typing.values.operator_definition import OperatorDefinition
from acab.modules.analysis.typing.values.type_definition import TypeDefinition

from acab.working_memory.trie_wm.parsing import ActionParser as AP
from acab.working_memory.trie_wm.parsing import FactParser as FP

def S(*in_string):
    return Sentence([AcabValue(x) for x in in_string])


class TypingTests(unittest.TestCase):

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
        type_def = TypeDefinition([])
        loc = FP.parseString('a.test.definition.x')[0]
        copied_loc = loc.attach_statement(type_def)
        self.assertEqual(len(tc._definitions), len(TypeInstance.Primitives))
        tc.add_definition(copied_loc)
        self.assertEqual(len(tc._definitions), len(TypeInstance.Primitives) + 4)
        defs = tc._definitions.get_nodes(lambda x: isinstance(x.value, TypeDefinition))
        self.assertEqual(len(TypeInstance.Primitives) + 1, len(defs))

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
        sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("a"))
        tc.add_assertion(sen)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)
        sen2 = S("a","c")
        sen2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen2)
        self.assertEqual(len(tc._get_known_typed_nodes()), 1)

    def test_basic_query(self):
        """ ::a END, a.$b """
        tc = TypeChecker()
        loc = FP.parseString('a.test.definition.$x')[0].attach_statement(TypeDefinition([]))
        tc.add_definition(loc)
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
        loc = FP.parseString('a.test.definition.$x')[0].attach_statement(TypeDefinition([]))
        tc.add_definition(loc)
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen1)

        query_sen = S("a","c")
        query_sen[-1]._data[TU.BIND_S] = True
        results = tc.query(query_sen)
        self.assertEqual(len(results), 0)

    def test_basic_inference(self):
        """ ::a END, test.$b(::a), test.$c """
        tc = TypeChecker()
        a_def = S("a").attach_statement(TypeDefinition([]))
        tc.add_definition(a_def)

        sen1 = S("test","b")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("a"))
        tc.add_assertion(sen1)

        sen2 = S("test","c")
        sen2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen2)

        query_sen = S("test","c")
        query_sen[-1]._data[TU.BIND_S] = True
        query_result = tc.query(query_sen)[0]
        self.assertEqual(query_result._type_instance, ATOM)

        tc.validate()
        query_result = tc.query(query_sen)[0]

        self.assertIsNotNone(query_result._type_instance)
        self.assertEqual(query_result._type_instance, sen1[-1].type)


    def test_type_conflict(self):
        """ σ::a END, σ::b END test.$q(::a), test.$q(::b) """
        tc = TypeChecker()
        a_def = S("a").attach_statement(TypeDefinition([]))
        b_def = S("b").attach_statement(TypeDefinition([]))
        tc.add_definition(a_def, b_def)
        sen1 = S("test","q")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("a"))
        tc.add_assertion(sen1)
        sen2 = S("test","q")
        sen2[-1]._data[TU.BIND_S] = True
        sen2[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("b"))

        with self.assertRaises(te.TypeConflictException):
            tc.add_assertion(sen2)

    def test_type_undefined(self):
        """ a.$b(::a) """
        tc = TypeChecker()
        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("a"))
        tc.add_assertion(sen1)

        with self.assertRaises(te.TypeUndefinedException):
            tc.validate()

    def test_type_redefinition(self):
        """ ::a END, ::a END """
        tc = TypeChecker()
        a_def = S("a").attach_statement(TypeDefinition([]))
        tc.add_definition(a_def)
        with self.assertRaises(te.TypeRedefinitionException):
            duplicate_a_def = S("a").attach_statement(TypeDefinition([S("test")]))
            tc.add_definition(duplicate_a_def)

    def test_type_duplicate_definition(self):
        """ ::a END, ::a END """
        tc = TypeChecker()
        a_def = S("a", "b").attach_statement(TypeDefinition([]))
        a_def2 = S("a", "b").attach_statement(TypeDefinition([]))
        tc.add_definition(a_def)
        tc.add_definition(a_def2)

        self.assertEqual(len(tc._definitions), len(TypeInstance.Primitives) + 2)


    def test_variable_conflict(self):
        """ ::String: END ::Number: END
        a.$x(::.String) b.$x(::.Number)
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        sen1 = S("a","b")
        sen1[-1]._data[TU.BIND_S] = True
        sen1[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))
        tc.add_assertion(sen1)
        sen2 = S("a","b")
        sen2[-1]._data[TU.BIND_S] = True
        sen2[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("Number"))
        with self.assertRaises(te.TypeConflictException):
            tc.add_assertion(sen2)

    def test_structure_mismatch(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::.String) END
        a(::first).b
        """
        tc = TypeChecker()
        a_def = S("a").attach_statement(TypeDefinition([]))
        b_def = S("b").attach_statement(TypeDefinition([]))
        tc.add_definition(a_def, b_def)

        struct_sen = S("name","x")
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))


        first_def = S("first").attach_statement(TypeDefinition([struct_sen]))
        tc.add_definition(first_def)

        sen = S("a","b")
        sen[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("first"))

        tc.add_assertion(sen)

        with self.assertRaises(te.TypeStructureMismatch):
            tc.validate()

    def test_structure_type_conflict(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::String) END
        a(::first).name.$y(::Number)
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        struct_sen = S("name","x")
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))
        first_def = S("first").attach_statement(TypeDefinition([struct_sen]))

        tc.add_definition(first_def)

        sen = S("a","name","y")
        sen[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("first"))
        sen[-1]._data[TU.BIND_S] = True
        sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("Number"))

        tc.add_assertion(sen)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()


    def test_typing_nested_vars(self):
        """ ::String: END, ::Number: END
        ::first.type: name.$x(::String).$y(::Number) END
        .bob(::first.type).name.$z.$q
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)


        struct_sen = S("name","x","y")
        struct_sen[-2]._data[TU.BIND_S] = True
        struct_sen[-2]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))
        struct_sen[-1]._data[TU.BIND_S] = True
        struct_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("Number"))
        first_def = S("first").attach_statement(TypeDefinition([struct_sen]))
        tc.add_definition(first_def)

        sen = S("bob","name","z","q")
        sen[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("first"))
        sen[-2]._data[TU.BIND_S] = True
        sen[-1]._data[TU.BIND_S] = True
        tc.add_assertion(sen)

        #Query the First Variable, should be untyped
        query_sen = S("bob","name","z")
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type_instance, ATOM)
        #then the second, should be untyped
        query_sen = S("bob","name","z","q")
        query_sen[-1]._data[TU.BIND_S] = True
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type_instance, ATOM)

        tc.validate()
        #Check the first var is inferred
        query_sen = S("bob","name","z")
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type_instance, TypeInstance(S("String")))
        #Check the second var is inferred
        query_sen = S("bob","name","z","q")
        query_sen[-2]._data[TU.BIND_S] = True
        query_sen[-1]._data[TU.BIND_S] = True
        self.assertEqual(tc.query(query_sen)[0]._type_instance, TypeInstance(S("Number")))

    def test_typing_nested_types(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        #Small Type
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_1_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))
        small_def = S("small", "type").attach_statement(TypeDefinition([type_1_sen]))
        tc.add_definition(small_def)

        #Large Type
        type_2_sen = S("component","x")
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("small", "type"))
        large_def = S("large", "type").attach_statement(TypeDefinition([type_2_sen]))

        tc.add_definition(large_def)

        assertion = S("a","component","q","name","w")
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("large", "type"))
        assertion[2]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.BIND_S] = True

        tc.add_assertion(assertion)

        query_sen1 = S("a","component","q")
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = S("a","component","q","name","w")
        query_sen2[-3]._data[TU.BIND_S] = True
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, ATOM)
        self.assertEqual(tc.query(query_sen2)[0]._type_instance, ATOM)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, TypeInstance(S("small", "type")))
        self.assertEqual(tc.query(query_sen2)[0]._type_instance, TypeInstance(S("String")))

    def test_typing_nested_types_fail(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w(::Number)
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        #Small Type
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_1_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))

        small_def = S("small", "type").attach_statement(TypeDefinition([type_1_sen]))
        tc.add_definition(small_def)

        #Large Type
        type_2_sen = S("component","x")
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("small", "type"))
        large_def = S("large", "type").attach_statement(TypeDefinition([type_2_sen]))

        tc.add_definition(large_def)

        assertion = S("a","component","q","name","w")
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("large", "type"))
        assertion[2]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("Number"))
        tc.add_assertion(assertion)

        query_sen1 = S("a","component","q")
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = S("a","component","q","name","w")
        query_sen2[-3]._data[TU.BIND_S] = True
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, ATOM)
        self.assertEqual(tc.query(query_sen2)[0]._type_instance, TypeInstance(S("Number")))

        with self.assertRaises(te.TypeConflictException):
            tc.validate()


    def test_typing_polytype(self):
        """ ::String: END, ::Number: END
        ::polytype: | $x | name.$x END
        a(::polytype(::String)).name.$q
        b(::polytype(::Number)).name.$t
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)


        #polytype
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        poly_def = S("poly", "type").attach_statement(TypeDefinition([type_1_sen],
                                                                     params=[AcabValue('x')]))
        tc.add_definition(poly_def)

        #assertions
        assertion = S("a","name","q")
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type"), [TypeInstance(S("String"))])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        assertion2 = S("b","name","t")
        assertion2[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type"), [TypeInstance(S("Number"), ["Number"])])
        assertion2[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion2)

        #queries
        query_sen1 = S("a","name","q")
        query_sen1[-1]._data[TU.BIND_S] = True

        query_sen2 = S("b","name","t")
        query_sen2[-1]._data[TU.BIND_S] = True

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, ATOM)
        self.assertEqual(tc.query(query_sen2)[0]._type_instance, ATOM)

        tc.validate()
        self.assertEqual(tc.query(query_sen1)[0]._type_instance, TypeInstance(S("String")))
        self.assertEqual(tc.query(query_sen2)[0]._type_instance, TypeInstance(S("Number")))

    def test_typing_polytype_nested(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x]: name.$x END
        ::ptypeTwo[$y]: nested(::ptypeOne(::$y)) END
        a(::ptypeTwo(::String)).nested.name.$x
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        #polytype 1
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        poly_1_def = S("poly", "type", "one").attach_statement(TypeDefinition([type_1_sen], params=["x"]))
        tc.add_definition(poly_1_def)

        #polytype 2
        type_2_sen = S("nested")
        type_2_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type", "one"), ["y"])
        poly_2_def = S("poly", "type", "two").attach_statement(TypeDefinition([type_2_sen], params=["y"]))
        tc.add_definition(poly_2_def)

        #Assertion
        assertion = S("a","nested","name","z")
        assertion_param = TypeInstance(S("String"))
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type", "two"), [assertion_param])
        assertion[-1]._data[TU.BIND_S] = True
        tc.add_assertion(assertion)

        #queries
        query_sen1 = S("a","nested","name","z")
        query_sen1[-1]._data[TU.BIND_S] = True

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, ATOM)

        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, TypeInstance(S("String")))

    def test_typing_polytype_multi_param(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x, $y]: name.$x, age.$y END
        a(::ptypeOne(::String, ::Number)).name.$q
        a.age.$w
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        #polytype
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_2_sen = S("age","y")
        type_2_sen[-1]._data[TU.BIND_S] = True
        poly_def = S("poly", "type").attach_statement(TypeDefinition([type_1_sen, type_2_sen], params=["x", "y"]))
        tc.add_definition(poly_def)

        #assertions
        assertion = S("a","name","q")
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type"),
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

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, ATOM)
        self.assertEqual(tc.query(query_sen2)[0]._type_instance, ATOM)
        tc.validate()

        self.assertEqual(tc.query(query_sen1)[0]._type_instance, TypeInstance(S("String")))
        self.assertEqual(tc.query(query_sen2)[0]._type_instance, TypeInstance(S("Number")))

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
        """
        ::String: END, ::Number: END
        ::polytype: | $x | name.$x END
        a(::polytype(::String)).name.$q(::Number)
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)


        # define poly type
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        poly_def = S("polytype").attach_statement(TypeDefinition([type_1_sen], params=[AcabValue("x")]))
        tc.add_definition(poly_def)


        # assert
        assertion = S("a","name","q")
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("polytype"), [TypeInstance(S("String"))])
        assertion[-1]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("Number"))
        tc.add_assertion(assertion)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()

    def test_typing_polytype_nested_fail(self):
        """
        ::String: END, ::Number: END
        ::polytype.small: | $x | name.$x END
        ::polytype.large: | $x | sub(::polytype.small(::$x)) END
        a(::polytype.large(::String)).sub.name.$q(::Number)
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        #polytype 1
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        poly_1_def = S("poly", "type", "one").attach_statement(TypeDefinition([type_1_sen], params=["x"]))
        tc.add_definition(poly_1_def)

        #polytype 2
        type_2_sen = S("nested")
        type_2_sen[-1]._data[TU.BIND_S] = True
        type_2_sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type", "one"), ["y"])
        poly_2_def = S("poly", "type", "two").attach_statement(TypeDefinition([type_2_sen], params=["y"]))
        tc.add_definition(poly_2_def)

        #Assertion
        assertion = S("a","nested","name","x")
        assertion[0]._data[TU.BIND_S] = True
        assertion_param = TypeInstance(S("String"))
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type", "two"), [assertion_param])
        assertion[-1]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("Number"))
        tc.add_assertion(assertion)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()

    def test_typing_polytype_multi_param_fail(self):
        """
        ::String: END, ::Number: END
        ::polytype.small: | $x, $y |
    		name.$x
        	age.$y
        END
        ::polytype.large: | $x, $y | sub(::polytype.small(::$x, ::$y)) END
        a(::polytype.large(::String, ::Number)).sub.name.$q(::String)
        a.sub.age.$w(::String)
        """
        tc = TypeChecker()
        str_def = S("String").attach_statement(TypeDefinition([]))
        num_def = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_def, num_def)

        #polytype
        type_1_sen = S("name","x")
        type_1_sen[-1]._data[TU.BIND_S] = True
        type_2_sen = S("age","y")
        type_2_sen[-1]._data[TU.BIND_S] = True
        poly_def = S("poly", "type").attach_statement(TypeDefinition([type_1_sen, type_2_sen], params=["x","y"]))
        tc.add_definition(poly_def)

        #assertions
        assertion = S("a","name","q")
        assertion[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("poly", "type"),
                                                         [TypeInstance(S("String")),
                                                          TypeInstance(S("Number"))])
        assertion[-1]._data[TU.BIND_S] = True
        assertion[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))
        tc.add_assertion(assertion)

        assertion2 = S("a","age","w")
        assertion2[-1]._data[TU.BIND_S] = True
        assertion2[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("String"))
        tc.add_assertion(assertion2)

        with self.assertRaises(te.TypeConflictException):
            tc.validate()


    def test_polytype_lacking_param(self):
        """
        ::simple.type: (::σ) end
        ::polytype: (::σ) | $x | name.$x END
        missing(::polytype).name.test(::simple.type)
        """
        # this should apply generalisation
        # ie: missing(::polytype).name.$y(::String) -> ::polytype(::String)?
        tc = TypeChecker()
        simple_type = S("simple","type").attach_statement(TypeDefinition([]))
        tc.add_definition(simple_type)

        sen_struct = S("name","x")
        sen_struct[-1]._data[TU.BIND_S] = True
        poly_def = S("a","polytype").attach_statement(TypeDefinition([sen_struct], params=["x"]))
        tc.add_definition(poly_def)

        sen = S("missing","name","blah")
        sen[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("a", "polytype"))
        sen[-1]._data[TU.VALUE_TYPE_S] = TypeInstance(S("simple","type"))

        tc.add_assertion(sen)

        tc.validate()

        result = tc.query(S("missing"))
        self.assertIsNotNone(result[0].type_instance.vars[0])


    @unittest.skip("TODO")
    def test_polytype_nested_lacking_param(self):
        # TODO
        return


    @unittest.skip("TODO")
    def test_add_rule(self):
        # TODO
	    return


    def test_add_operation(self):
        """ ::Number end
        λ::AddOp: $x(::Number).$x.$x
        """
        tc = TypeChecker()
        str_type = S("String").attach_statement(TypeDefinition([]))
        num_type = S("Number").attach_statement(TypeDefinition([]))
        tc.add_definition(str_type, num_type)

        op_struct = S("x", "x", "x")
        op_struct[0]._data[TU.BIND_S] = True
        op_struct[0]._data[TU.VALUE_TYPE_S] = TypeInstance(S("Number"))
        op_struct[1]._data[TU.BIND_S] = True
        op_struct[2]._data[TU.BIND_S] = True

        op_def = S("AddOp").attach_statement(OperatorDefinition(op_struct))
        tc.add_definition(op_def)

        #Add an operation use
        trans_params = S("x", "y")
        trans_params[0]._data[TU.BIND_S] = True
        trans_params[1]._data[TU.BIND_S] = True
        rebind = S("z")[0]
        rebind._data[TU.BIND_S] = True
        transform = TransformComponent(Sentence.build(["AddOp"]), trans_params, rebind=rebind)

        [tc.add_assertion(x) for x in transform.to_abstract_sentences()]

        tc.validate()
        # TODO




    @unittest.skip("TODO")
    def test_infer_from_operation(self):
        # TODO
        return


    @unittest.skip("TODO")
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
