import unittest
import logging as root_logger
from os.path import split, splitext
logging = root_logger.getLogger(__name__)


from acab.abstract.config.config import AcabConfig
AcabConfig.Get("acab/abstract/config")

from acab.abstract.core.values import Sentence
from acab.abstract.core.values import AcabValue

from acab.abstract.core.node import AcabNode
from acab.abstract.containers import action
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionComponent
from acab.abstract.parsing.bootstrap_parser import BootstrapParser

from acab.modules.analysis.typing import type_exceptions as te
from acab.modules.analysis.typing import util as TU
from acab.modules.analysis.typing.parsing import TypeDefParser as TD
from acab.modules.analysis.typing.type_checker import TypeChecker
from acab.modules.analysis.typing.typing_module import TypingSpec
from acab.modules.analysis.typing.values.operator_definition import OperatorDefinition
from acab.modules.analysis.typing.values.type_definition import TypeDefinition

from acab.modules.parsing.el_parsing ActionParser as AP
from acab.modules.parsing.el_parsing TransformParser as TP
from acab.modules.parsing.el_parsing FactParser as FP
from acab.working_memory.trie_wm.trie_working_memory import TrieWM


def S(*in_string):
    return Sentence.build([AcabValue(x) for x in in_string])

class TypingCombinedTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        bp = BootstrapParser()
        twm = TrieWM()
        ts = TypingSpec()
        twm.assert_parsers(bp)
        ts.assert_parsers(bp)
        ts.query_parsers(bp)
        twm.query_parsers(bp)

    def setUp(self):
        self.tc = TypeChecker()

    def tearDown(self):
        return 1

    #----------
    def test_init_parse(self):
        def1 = TD.parseString("blah.x: (::σ) end")[0]
        inst = def1[-1].build_type_instance()
        self.tc.add_definition(def1)

        sen1 = FP.parseString("a.b.c(::blah.x)")[0]
        self.tc.add_assertion(sen1)
        self.tc.validate()

        query1 = FP.parseString("a.b.c")[0]
        result = self.tc.query(query1)
        self.assertEqual(result[0].type_instance, inst)


    @unittest.skip("Need to fix primitive initialisation in type checker")
    def test_add_definition(self):
        """ :: a END """
        n_primitives = 0
        type_def = TD.parseString("a.test.definition.x: (::σ) end")[0]
        self.assertEqual(len(self.tc._definitions), n_primitives)
        self.tc.add_definition(type_def)
        self.assertEqual(len(self.tc._definitions), n_primitives + 4)
        defs = self.tc._definitions.get_nodes(lambda x: isinstance(x.value, TypeDefinition))
        self.assertEqual(n_primitives + 1, len(defs))

    def test_add_assertion(self):
        """ a.b.c.d """
        self.assertEqual(len(self.tc._assignments), 0)
        self.tc.add_assertion(FP.parseString("a.b.c.d")[0])
        self.assertEqual(len(self.tc._assignments), 4)

    def test_add_operation(self):
        """ ::Number end
        λ::AddOp: $x(::Number).$x.$x
        """
        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        op_def = TD.parseString("AddOp: (::λ) $x(::number).$x.$x")[0]
        self.tc.add_definition(op_def)

        #Add an operation use
        trans_params = S("x", "y")
        trans_params[0]._data[TU.BIND_S] = True
        trans_params[1]._data[TU.BIND_S] = True
        rebind = S("z")[0]
        rebind._data[TU.BIND_S] = True
        transform = ProductionComponent("AddOp", trans_params, rebind=rebind)

        [self.tc.add_assertion(x) for x in transform.to_sentences()]

        self.tc.validate()

        # TODO verify


    def test_get_known_typed_nodes(self):
        """ a.$b(::a), a.$c """

        self.assertFalse(self.tc._get_known_typed_nodes())
        sen = FP.parseString("a.$b(::a)")[0]
        self.tc.add_assertion(sen)
        self.assertEqual(len(self.tc._get_known_typed_nodes()), 1)
        sen2 = FP.parseString("a.$c")[0]
        self.tc.add_assertion(sen2)
        self.assertEqual(len(self.tc._get_known_typed_nodes()), 1)

    def test_basic_query(self):
        """ ::a END, a.$b """
        type_def = TD.parseString("a.test.definition.x: (::σ) end")[0]
        self.tc.add_definition(type_def)
        sen1 = FP.parseString("a.$b")[0]
        self.tc.add_assertion(sen1)

        query_sen = FP.parseString("a.$b")[0]
        results = self.tc.query(query_sen)
        self.assertEqual(len(results), 1)

    def test_basic_query_fail(self):
        """ ::a END, a.$b """

        type_def = TD.parseString("a.test.definition.x: (::σ) end")[0]
        self.tc.add_definition(type_def)
        sen1 = FP.parseString("a.$b")[0]
        self.tc.add_assertion(sen1)

        query_sen = FP.parseString("a.$c")[0]
        results = self.tc.query(query_sen)
        self.assertEqual(len(results), 0)

    def test_basic_inference(self):
        """ ::a END, test.$b(::a), test.$c """

        a_def = TD.parseString("a: (::σ) end")[0]
        self.tc.add_definition(a_def)

        sen1 = FP.parseString("test.$b(::a)")[0]
        self.tc.add_assertion(sen1)

        sen2 = FP.parseString("test.$c")[0]
        self.tc.add_assertion(sen2)

        query_sen = FP.parseString("test.$c")[0]
        query_result = self.tc.query(query_sen)[0]
        self.assertEqual(query_result._type_instance, ATOM)

        self.tc.validate()
        query_result = self.tc.query(query_sen)[0]

        self.assertIsNotNone(query_result._type_instance)
        self.assertEqual(query_result._type_instance, sen1[-1].type)


    def test_type_conflict(self):
        """ σ::a END, σ::b END test.$q(::a), test.$q(::b) """

        a_def = TD.parseString("a: (::σ) end")[0]
        b_def = TD.parseString("b: (::σ) end")[0]
        self.tc.add_definition(a_def, b_def)
        sen1 = FP.parseString("test.$q(::a)")[0]
        self.tc.add_assertion(sen1)
        sen2 = FP.parseString("test.$q(::b)")[0]

        with self.assertRaises(te.TypeConflictException):
            self.tc.add_assertion(sen2)

    def test_type_undefined(self):
        """ a.$b(::a) """

        sen1 = FP.parseString("a.$b(::a)")[0]
        self.tc.add_assertion(sen1)

        with self.assertRaises(te.TypeUndefinedException):
            self.tc.validate()

    def test_type_redefinition(self):
        """ ::a END, ::a END """

        a_def = TD.parseString("a: (::σ) end")[0]
        self.tc.add_definition(a_def)
        with self.assertRaises(te.TypeRedefinitionException):
            duplicate_a_def = TD.parseString("a: (::σ)\na.b.c\nend")[0]
            self.tc.add_definition(duplicate_a_def)

    def test_type_duplicate_definition(self):
        """ ::a END, ::a END """

        a_def = TD.parseString("a.b: (::σ) end")[0]
        a_def2 = TD.parseString("a.b: (::σ) end")[0]
        self.tc.add_definition(a_def)
        self.tc.add_definition(a_def2)

        self.assertEqual(len(self.tc._definitions), len(Sentence.build.Primitives) + 2)


    def test_variable_conflict(self):
        """ ::String: END ::Number: END
        a.$x(::.String) b.$x(::.Number)
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        sen1 = FP.parseString("a.$b(::string)")[0]
        self.tc.add_assertion(sen1)
        sen2 = FP.parseString("a.$b(::number)")[0]
        with self.assertRaises(te.TypeConflictException):
            self.tc.add_assertion(sen2)

    def test_structure_mismatch(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::.String) END
        a(::first).b
        """

        a_def = TD.parseString("a: (::σ) end")[0]
        b_def = TD.parseString("b: (::σ) end")[0]
        self.tc.add_definition(a_def, b_def)

        first_def = TD.parseString("first: (::σ)\nname.$x(::a)\nend")[0]
        self.tc.add_definition(first_def)

        sen = FP.parseString("a(::first).b")[0]
        self.tc.add_assertion(sen)

        with self.assertRaises(te.TypeStructureMismatch):
            self.tc.validate()

    def test_structure_type_conflict(self):
        """ ::String: END, ::Number: END
        ::first: name.$x(::String) END
        a(::first).name.$y(::Number)
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        first_def = TD.parseString("first: (::σ)\nname.$x(::string)\nend")[0]
        self.tc.add_definition(first_def)

        sen = FP.parseString("a(::first).name.$y(::number)")[0]
        self.tc.add_assertion(sen)

        with self.assertRaises(te.TypeConflictException):
            self.tc.validate()


    def test_typing_nested_vars(self):
        """ ::String: END, ::Number: END
        ::first.type: name.$x(::String).$y(::Number) END
        .bob(::first.type).name.$z.$q
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)


        first_def = TD.parseString("first: (::σ)\nname.$x(::string).$y(::number)\nend")[0]
        self.tc.add_definition(first_def)

        sen = FP.parseString("bob(::first).name.$z.$q")[0]
        self.tc.add_assertion(sen)

        #Query the First Variable, should be untyped
        query_sen = FP.parseString("bob.name.$z")[0]
        self.assertEqual(self.tc.query(query_sen)[0]._type_instance, ATOM)
        #then the second, should be untyped
        query_sen = FP.parseString("bob.name.$z.$q")[0]
        self.assertEqual(self.tc.query(query_sen)[0]._type_instance, ATOM)

        self.tc.validate()
        #Check the first var is inferred
        query_sen = FP.parseString("bob.name.$z")[0]
        str_instance = str_def[-1].build_type_instance()
        self.assertEqual(self.tc.query(query_sen)[0]._type_instance, str_instance)
        #Check the second var is inferred
        query_sen = FP.parseString("bob.name.$z.$q")[0]
        num_instance = num_def[-1].build_type_instance()
        self.assertEqual(self.tc.query(query_sen)[0]._type_instance, num_instance)

    def test_typing_nested_types(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        #Small Type
        small_def = TD.parseString("small.type: (::σ)\nname.$x(::string)\nend")[0]
        self.tc.add_definition(small_def)

        #Large Type
        large_def = TD.parseString("large.type: (::σ)\ncomponent.$x(::small.type)\nend")[0]
        self.tc.add_definition(large_def)

        assertion = FP.parseString("a(::large.type).component.$q.name.$w")[0]
        self.tc.add_assertion(assertion)

        query_sen1 = FP.parseString("a.component.$q")[0]
        query_sen2 = FP.parseString("a.component.$q.name.$w")[0]

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, ATOM)
        self.assertEqual(self.tc.query(query_sen2)[0]._type_instance, ATOM)

        self.tc.validate()

        instance = small_def[-1].build_type_instance()

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, instance)
        self.assertEqual(self.tc.query(query_sen2)[0]._type_instance, Sentence.build(S("string")))

    def test_typing_nested_types_fail(self):
        """ ::String: END, ::Number: END
        ::small.type: name.$x(::String) END
        ::large.type: component.$x(::small.type) END
        a(::large.type).component.$q.name.$w(::Number)
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        #Small Type
        small_def = TD.parseString("small.type: (::σ)\nname.$x(::string)\nend")[0]
        self.tc.add_definition(small_def)

        #Large Type
        large_def = TD.parseString("large.type: (::σ)\ncomponent.$x(::small.type)\nend")[0]
        self.tc.add_definition(large_def)

        assertion = FP.parseString("a(::large.type).component.$q.name.$w(::number)")[0]
        self.tc.add_assertion(assertion)

        query_sen1 = FP.parseString("a.component.$q")[0]

        query_sen2 = FP.parseString("a.component.$q.name.$w")[0]

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, ATOM)

        num_instance = num_def[-1].build_type_instance()
        self.assertEqual(self.tc.query(query_sen2)[0]._type_instance, num_instance)

        with self.assertRaises(te.TypeConflictException):
            self.tc.validate()


    def test_typing_polytype(self):
        """ ::String: END, ::Number: END
        ::polytype: | $x | name.$x END
        a(::polytype(::String)).name.$q
        b(::polytype(::Number)).name.$t
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)


        #polytype
        poly_def = TD.parseString("poly.type: (::σ)\n| $x |\n\nname.$x\nend")[0]
        self.tc.add_definition(poly_def)

        #assertions
        assertion = FP.parseString("a(::poly.type(::string)).name.$q")[0]
        self.tc.add_assertion(assertion)

        assertion2 = FP.parseString("b(::poly.type(::number)).name.$t")[0]
        self.tc.add_assertion(assertion2)

        #queries
        query_sen1 = FP.parseString("a.name.$q")[0]

        query_sen2 = FP.parseString("b.name.$t")[0]

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, ATOM)
        self.assertEqual(self.tc.query(query_sen2)[0]._type_instance, ATOM)

        self.tc.validate()
        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, Sentence.build(S("string")))
        self.assertEqual(self.tc.query(query_sen2)[0]._type_instance, Sentence.build(S("number")))

    def test_typing_polytype_nested(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x]: name.$x END
        ::ptypeTwo[$y]: nested(::ptypeOne(::$y)) END
        a(::ptypeTwo(::String)).nested.name.$x
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        #polytype 1
        poly_1_def = TD.parseString("poly.type.one: (::σ)\n | $x |\n\nname.$x\nend")[0]
        self.tc.add_definition(poly_1_def)

        #polytype 2
        poly_2_def = TD.parseString("poly.type.two: (::σ)\n | $y |\n\nnested(::poly.type.one($y))\nend")[0]
        self.tc.add_definition(poly_2_def)

        #Assertion
        assertion = FP.parseString("a(::poly.type.two(::string)).nested.name.$z")[0]
        self.tc.add_assertion(assertion)

        #queries
        query_sen1 = FP.parseString("a.nested.name.$z")[0]

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, ATOM)

        self.tc.validate()

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, Sentence.build(S("string")))

    def test_typing_polytype_multi_param(self):
        """ ::String: END, ::Number: END
        ::ptypeOne[$x, $y]: name.$x, age.$y END
        a(::ptypeOne(::String, ::Number)).name.$q
        a.age.$w
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        #polytype
        poly_def = TD.parseString("poly.type: (::σ)\n | $x, $y |\n\nname.$x\nage.$y\nend")[0]
        self.tc.add_definition(poly_def)

        #assertions
        assertion = FP.parseString("a(::poly.type(::string, ::number)).name.$q")[0]
        self.tc.add_assertion(assertion)

        assertion2 = FP.parseString("a.age.$w")[0]
        self.tc.add_assertion(assertion2)

        #queries
        query_sen1 = FP.parseString("a.name.$q")[0]

        query_sen2 = FP.parseString("a.age.$w")[0]

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, ATOM)
        self.assertEqual(self.tc.query(query_sen2)[0]._type_instance, ATOM)
        self.tc.validate()

        self.assertEqual(self.tc.query(query_sen1)[0]._type_instance, Sentence.build(S("string")))
        self.assertEqual(self.tc.query(query_sen2)[0]._type_instance, Sentence.build(S("number")))

    def test_typing_context_clear(self):
        sen = FP.parseString("a.test.$var")[0]
        self.tc.add_assertion(sen)

        sen2 = FP.parseString("$var.blah")[0]
        self.tc.add_assertion(sen2)

        self.assertEqual(len(self.tc._variables), 1)
        self.assertIsNotNone(self.tc.query(sen)[0]._var_node)
        self.assertEqual(len(self.tc.query(sen2)), 1)

        self.tc.clear_context()

        self.assertEqual(len(self.tc._variables), 0)
        self.assertIsNone(self.tc.query(sen)[0]._var_node)
        self.assertEqual(len(self.tc.query(sen2)), 0)


    def test_typing_polytype_fail(self):
        """
        ::String: END, ::Number: END
        ::polytype: | $x | name.$x END
        a(::polytype(::String)).name.$q(::Number)
        """

        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        # define poly type
        poly_def = TD.parseString("polytype: (::σ)\n| $x |\n\nname.$x\nend")[0]
        self.tc.add_definition(poly_def)


        # assert
        assertion = FP.parseString("a(::polytype(::string)).name.$q(::number)")[0]
        self.tc.add_assertion(assertion)

        with self.assertRaises(te.TypeConflictException):
            self.tc.validate()

    def test_typing_polytype_nested_fail(self):
        """
        ::String: END, ::Number: END
        ::polytype.small: | $x | name.$x END
        ::polytype.large: | $x | sub(::polytype.small(::$x)) END
        a(::polytype.large(::String)).sub.name.$q(::Number)
        """
        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        #polytype 1
        poly_1_def = TD.parseString("poly.type.one: (::σ)\n | $x |\n\nname.$x\nend")[0]
        self.tc.add_definition(poly_1_def)

        #polytype 2
        poly_2_def = TD.parseString("poly.type.two: (::σ)\n | $y |\n\nnested(::poly.type.one($y))\nend")[0]
        self.tc.add_definition(poly_2_def)

        #Assertion
        assertion = FP.parseString("a(::poly.type.two(::string)).nested.name.$x(::number)")[0]
        self.tc.add_assertion(assertion)

        with self.assertRaises(te.TypeConflictException):
            self.tc.validate()

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
        str_def = TD.parseString("string: (::σ) end")[0]
        num_def = TD.parseString("number: (::σ) end")[0]
        self.tc.add_definition(str_def, num_def)

        #polytype
        poly_def = TD.parseString("poly.type: (::σ)\n | $x, $y |\n\nname.$x\nage.$y\nend")[0]
        self.tc.add_definition(poly_def)

        #assertions
        assertion = FP.parseString("a(::poly.type(::string, ::number)).name.$q(::string)")[0]
        self.tc.add_assertion(assertion)

        assertion2 = FP.parseString("a.age.$w(::string)")[0]
        self.tc.add_assertion(assertion2)

        with self.assertRaises(te.TypeConflictException):
            self.tc.validate()

    def test_polytype_generalise(self):
        """
        ::simple.type: (::σ) end
        ::polytype: (::σ) | $x | name.$x END
        missing(::polytype).name.test(::simple.type)
        """
        # this should apply generalisation
        # ie: missing(::polytype).name.$y(::String) -> ::polytype(::String)?

        simple_type = TD.parseString("simple.type: (::σ) end")[0]
        self.tc.add_definition(simple_type)

        poly_def = TD.parseString("a.polytype: (::σ)\n| $x |\n\nname.$x\nend")[0]
        self.tc.add_definition(poly_def)

        sen = FP.parseString("missing(::a.polytype).name.blah(::simple.type)")[0]
        self.tc.add_assertion(sen)

        self.tc.validate()

        result = self.tc.query(S("missing"))
        self.assertEqual(result[0].type_instance.vars[0], simple_type[-1].build_type_instance())


    def test_sum_type(self):
        """
        a.sum.type: (::Σσ)\n a: (::σ) end\n b: (::σ) end\n end

        first(::a.sum.type.a)
        second(::a.sum.type.b)
        """
        sum_type = TD.parseString("a.sum.type: (::Σσ)\na: (::σ) end\n\nb: (::σ) end\nend")

        self.tc.add_definition(*sum_type)

        assertion_1 = FP.parseString("first!a(::a.sum.type)")[0]
        self.tc.add_assertion(assertion_1)

        assertion_2 = FP.parseString("second!b(::a.sum.type)")[0]
        self.tc.add_assertion(assertion_2)

        self.tc.validate()

        result = self.tc.query(S("first", "a"))
        self.assertEqual(result[0].type_instance, sum_type[0][-1].build_type_instance())

        result_2 = self.tc.query(S("second", "b"))
        self.assertEqual(result_2[0].type_instance, sum_type[0][-1].build_type_instance())

    def test_sum_type_mismatch(self):
        """
        a.sum.type: (::Σσ)\n a: (::σ) end\n b: (::σ) end\n end

        first(::a.sum.type.a)
        second(::a.sum.type.b)
        """
        sum_type = TD.parseString("a.sum.type: (::Σσ)\na: (::σ) end\n\nb: (::σ) end\nend")

        self.tc.add_definition(*sum_type)

        assertion_1 = FP.parseString("first!blah(::a.sum.type)")[0]
        self.tc.add_assertion(assertion_1)

        with self.assertRaises(te.TypeStructureMismatch):
            self.tc.validate()

    def test_sum_type_sub_product(self):
        """
        a.sum.type: (::Σσ)
            a: (::σ)
                blah.$x
                bloo.$y
            end
            b: (::σ) end
        end

        first(::a.sum.type.a)
        second(::a.sum.type.b)
        """
        sum_type = TD.parseString("a.sum.type: (::Σσ)\na: (::σ)\nblah.$x\nbloo.$y\n end\n\nb: (::σ) end\nend")

        self.tc.add_definition(*sum_type)

        assertion_1 = FP.parseString("first!a(::a.sum.type).bloo.awef")[0]
        self.tc.add_assertion(assertion_1)

        assertion_2 = FP.parseString("second!b(::a.sum.type)")[0]
        self.tc.add_assertion(assertion_2)

        self.tc.validate()

        result = self.tc.query(S("first", "a"))
        self.assertEqual(result[0].type_instance, sum_type[0][-1].build_type_instance())

        result_2 = self.tc.query(S("second", "b"))
        self.assertEqual(result_2[0].type_instance, sum_type[0][-1].build_type_instance())

    def test_sum_type_sub_product_mismatch(self):
        """
        a.sum.type: (::Σσ)
            a: (::σ)
                blah.$x
                bloo.$y
            end
            b: (::σ) end
        end

        first(::a.sum.type.a)
        second(::a.sum.type.b)
        """
        sum_type = TD.parseString("a.sum.type: (::Σσ)\na: (::σ)\nblah.$x\nbloo.$y\n end\n\nb: (::σ) end\nend")

        self.tc.add_definition(*sum_type)

        assertion_1 = FP.parseString("first!a(::a.sum.type).awef.awef")[0]
        self.tc.add_assertion(assertion_1)

        with self.assertRaises(te.TypeStructureMismatch):
            self.tc.validate()

    def test_sum_type_polymorphic(self):
        """
        a.sum.type: (::Σσ)
           | $x |
            a: (::σ)
                blah.$x
            end
            b: (::σ) end
        end

        first(::a.sum.type.a)
        second(::a.sum.type.b)
        """
        simple_type = TD.parseString("simple.type: (::σ) end")
        self.tc.add_definition(*simple_type)
        other_type = TD.parseString("other.type: (::σ) end")
        self.tc.add_definition(*other_type)

        sum_type = TD.parseString("a.sum.type: (::Σσ)\n| $x |\n\na: (::σ)\nblah.$x\n\n end\n\nb: (::σ) end\nend")
        self.tc.add_definition(*sum_type)

        assertion_1 = FP.parseString("first!a(::a.sum.type(::simple.type)).blah.aweg")[0]
        self.tc.add_assertion(assertion_1)

        self.tc.validate()

        result = self.tc.query(S("first", "a"))
        self.assertEqual(result[0].type_instance, sum_type[0][-1].build_type_instance({'x':simple_type[0][-1].build_type_instance()}))

    def test_sum_type_polymorphic_mismatch(self):
        """
        a.sum.type: (::Σσ)
           | $x |
            a: (::σ)
                blah.$x
            end
            b: (::σ) end
        end

        first(::a.sum.type.a)
        second(::a.sum.type.b)
        """
        simple_type = TD.parseString("simple.type: (::σ) end")
        self.tc.add_definition(*simple_type)
        other_type = TD.parseString("other.type: (::σ) end")
        self.tc.add_definition(*other_type)

        sum_type = TD.parseString("a.sum.type: (::Σσ)\n| $x |\n\na: (::σ)\nblah.$x\n\n end\n\nb: (::σ) end\nend")
        self.tc.add_definition(*sum_type)

        assertion_1 = FP.parseString("first!a(::a.sum.type(::simple.type)).blah.aweg(::other.type)")[0]
        self.tc.add_assertion(assertion_1)

        with self.assertRaises(te.TypeConflictException):
            self.tc.validate()

    def test_sum_type_polymorphic_generalise(self):
        """
        a.sum.type: (::Σσ)
           | $x |
            a: (::σ)
                blah.$x
            end
            b: (::σ) end
        end

        first!a(::a.sum.type).blah.awef(::other.type)
        ->
        first!a(::a.sum.type(::other.type))
        """
        simple_type = TD.parseString("simple.type: (::σ) end")
        self.tc.add_definition(*simple_type)
        other_type = TD.parseString("other.type: (::σ) end")
        self.tc.add_definition(*other_type)

        sum_type = TD.parseString("a.sum.type: (::Σσ)\n| $x |\n\na: (::σ)\nblah.$x\n\n end\n\nb: (::σ) end\nend")
        self.tc.add_definition(*sum_type)

        assertion_1 = FP.parseString("first!a(::a.sum.type).blah.aweg(::other.type)")[0]
        self.tc.add_assertion(assertion_1)

        assertion_2 = FP.parseString("second!a(::a.sum.type).blah.aweg(::simple.type)")[0]
        self.tc.add_assertion(assertion_2)

        self.tc.validate()

        result = self.tc.query(FP.parseString("first!a")[0])
        self.assertEqual(result[0].type_instance.vars[0], other_type[0][-1].build_type_instance())

        result_2 = self.tc.query(FP.parseString("second!a")[0])
        self.assertEqual(result_2[0].type_instance.vars[0], simple_type[0][-1].build_type_instance())


    # Test operators:
    # Define an operator
    # infer types from operator
    # define multiple operators
    # spec operator alias'
    # infer operators from alias
    # infer from amongst multiple possible operators


    def test_add_operation(self):
        """ ::Number end
        λ::AddOp: $x(::Number).$x.$x
        """

        str_type = TD.parseString("a.string: (::σ) end")
        num_type = TD.parseString("a.num: (::σ) end")
        self.tc.add_definition(*str_type, *num_type)

        op_def = TD.parseString("operator.transform.format: (::λ) $x(::a.string).$x.$x")
        self.tc.add_definition(*op_def)

        #Add an operation use
        transform = TP.parseString("λoperator.transform.format $a $b -> $c")

        self.tc.add_assertion(*transform.to_sentences())

        self.tc.validate()

        # TODO verify
        print(self.tc)


    def test_add_operation_alias(self):
        """ ::Number end
        λ::AddOp: $x(::Number).$x.$x
        """
        str_type = TD.parseString("a.string: (::σ) end")
        num_type = TD.parseString("a.num: (::σ) end")
        self.tc.add_definition(*str_type, *num_type)

        op_def = TD.parseString("operator.transform.FormatOp: (::λ) $x(::a.num).$x.$x => ~=")
        self.tc.add_definition(*op_def)

        #Add an operation use
        transform = TP.parseString("$a ~= $b -> $c")

        self.tc.add_assertion(*transform.to_sentences())

        self.tc.validate()

        # TODO verify


    @unittest.skip("waiting on local structure unifying type params")
    def test_polytype_nested_generalise(self):
        """
        ::simple.type: (::σ) end
        ::polytype: (::σ) | $x | name.$x END
        missing(::polytype).name.test(::simple.type)
        """
        # this should apply generalisation
        # ie: missing(::polytype).name.$y(::String) -> ::polytype(::String)?

        simple_type = TD.parseString("simple.type: (::σ) end")[0]
        self.tc.add_definition(simple_type)

        poly_def_1 = TD.parseString("a.first.polytype: (::σ)\n| $x |\n\nname.$x\nend")[0]
        self.tc.add_definition(poly_def_1)

        poly_def_2 = TD.parseString("a.second.polytype: (::σ)\n| $x |\n\nsub(::a.first.polytype($x))\nend")[0]
        self.tc.add_definition(poly_def_2)

        sen = FP.parseString("missing(::a.second.polytype).sub.name.blah(::simple.type)")[0]
        self.tc.add_assertion(sen)

        self.tc.validate()

        result = self.tc.query(S("missing"))
        self.assertEqual(result[0].type_instance.vars[0], simple_type[0][-1].build_type_instance())

    @unittest.skip("TODO")
    def test_add_rule(self):
        # TODO
        return

    @unittest.skip("TODO")
    def test_infer_from_operation(self):
        # TODO
        return

    @unittest.skip("waiting on var -> type var coercion")
    def test_infer_polytype_param_from_use(self):
        """
        simple.type: (::σ) end
        poly.type: (::σ) | $x | name.$x end
        a(::poly.type($a)).name.$x(::simple.type)
        """
        simple_type = TD.parseString("simple.type: (::σ) end")[0]
        self.tc.add_definition(simple_type)

        poly_def = TD.parseString("a.polytype: (::σ)\n| $x |\n\nname.$x\nend")[0]
        self.tc.add_definition(poly_def)

        sen = FP.parseString("missing(::a.polytype($z)).name.blah(::simple.type)")[0]
        self.tc.add_assertion(sen)

        self.tc.validate()

        result = self.tc.query(S("missing"))

        # TODO verify


