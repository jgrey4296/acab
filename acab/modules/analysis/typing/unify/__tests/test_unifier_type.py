# https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from os.path import split, splitext

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    import pyparsing as pp
    config = acab.setup()
    from acab.core.parsing import pyparse_dsl as ppDSL
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)
    import acab.core.defaults.value_keys as DS
    from acab.core.parsing.annotation import ValueAnnotation
    from acab.interfaces.value import ValueFactory as VF
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.core.value.value import AcabValue
    from acab.interfaces import value as VI
    from acab.modules.analysis.typing.module import TypeSpecFragment
    from acab.modules.context.context_set import ContextInstance as CtxIns
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.modules.values.sen_val.module import Sen_Val_Parser
    from acab.modules.values.binding.binding import Bind

    from ... import exceptions as TE
    from .. import simple_unify_fns as suf
    from .. import type_unify_fns as tuf
    from .. import unifier as unify
    from .. import util


class UnifierTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser).register(TypeSpecFragment().build_dsl())
        cls.dsl.register(Sen_Val_Parser)
        cls.dsl.register(Component_DSL)
        cls.dsl.build()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------

    def test_subtype_relation_fail(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("d.b.c")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

    def test_subtype_relation_true_sub(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

    def test_subtype_relation_true_sub_reversed(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen2, sen1, CtxIns())

    def test_subtype_relation_right_var(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("a.b.$x")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "c")


    def test_subtype_relation_left_var(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertIn("x", ctx_r)

    def test_subtype_relation_left_var_crit_path(self):
        sen1 = self.dsl("a.$x.c")[0]
        sen2 = self.dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "b")


    def test_unify_non_var_types(self):
        sen1 = self.dsl("a.test.sentence")[0]
        sen2 = self.dsl("a.test.sentence(::blah)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertTrue(ctx_r)
        self.assertIn(str(sen1[:]), ctx_r)
        self.assertEqual(ctx_r[str(sen1[:])], ValueAnnotation(DS.TYPE_INSTANCE,
                                                              self.dsl("blah")[0]))

    def test_unify_types_var(self):
        sen1 = self.dsl("a.test.sentence")[0]
        sen2 = self.dsl("a.test.$x(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        self.assertTrue(ctx_r)
        self.assertEqual(ctx_r[sen1c[-1]].type, sen2[-1].type)
        self.assertEqual(sen1c[-1].type, "_:blah")

    def test_apply_types_var_left(self):
        sen1 = self.dsl("a.test.$x")[0]
        sen2 = self.dsl("a.test.sentence(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-1].type)

    def test_apply_types_var_left_repeated(self):
        sen1 = self.dsl("a.test.$x.$x")[0]
        sen2 = self.dsl("a.test.sentence(::blah)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-1].type)

    def test_types_var_left_repeated_conflict(self):
        sen1 = self.dsl("a.test.$x.$x(::bloo)")[0]
        sen2 = self.dsl("a.test.sentence(::blah).awf(::$y)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

    def test_types_var_right_repeated_conflict(self):
        sen1 = self.dsl("a.test.$x.$x(::bloo)")[0]
        sen2 = self.dsl("a.test.sentence(::blah).awf(::$y)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen2, sen1, CtxIns())


    def test_types_repeated_conflict(self):
        sen1 = self.dsl("a.test.val(::$x).val(::$x)")[0]
        sen2 = self.dsl("a.test.$x(::blah).$y(::bloo)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen2, sen1, CtxIns())

    def test_types_repeated_conflict_2(self):
        sen1 = self.dsl("a.test.val(::$x).val(::$x)")[0]
        sen2 = self.dsl("a.test.val(::blah).val(::bloo)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen2, sen1, CtxIns())


    def test_apply_type_var(self):
        sen1 = self.dsl("a.test.$x(::$y)")[0]
        sen2 = self.dsl("a.test.sentence(::blah.bloo)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        # self.assertEqual(len(ctx_r), 2)
        self.assertIn("x", ctx_r)
        # self.assertIn("y", ctx_r)

        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        self.assertEqual(sen1c, "_:a.test.sentence")
        self.assertEqual(sen1c[-1].type, "_:blah.bloo")


    def test_apply_types_generalise(self):
        sen1 = self.dsl("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = self.dsl("a.test(::blah).sentence(::a.b)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        sen2c = tuf.type_unify.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-2].type, "_:blah")
        self.assertEqual(sen1c[-1].type, "_:a.b")

    def test_apply_types_with_vars(self):
        sen1 = self.dsl("a.test.sentence")[0]
        sen2 = self.dsl("a.test.$x(::blah!$y)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        sen2c = tuf.type_unify.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-1].type, "_:blah.y")
        self.assertTrue(sen1c[-1].type[-1].is_var)

        self.assertIn(str(sen1[:]), ctx_r)
        self.assertIsInstance(ctx_r[str(sen1[:])], ValueAnnotation)
        self.assertEqual(ctx_r[str(sen1[:])].value, "_:blah.y")

    def test_apply_types_with_vars_completed(self):
        sen1 = self.dsl("a.test.sentence.bloo(::aweg.awg)")[0]
        sen2 = self.dsl("a.test.$x(::blah!$y).$z(::$y)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        sen2c = tuf.type_unify.apply(sen2, ctx_r)

        self.assertEqual(ctx_r.x, "sentence")
        self.assertEqual(ctx_r.y, "_:aweg.awg")
        self.assertEqual(sen1c[-2], "sentence")

        self.assertEqual(sen1c[-1].type, "_:aweg.awg")
        self.assertEqual(sen2c[-1].type, "_:aweg.awg")

        self.assertEqual(sen1c[-2].type, "_:blah.[aweg.awg]")
        self.assertEqual(sen2c[-2].type, "_:blah.[aweg.awg]")
        self.assertEqual(sen1c[-2].type[-1], "_:aweg.awg")
        self.assertEqual(sen2c[-2].type[-1][-1], "awg")

        self.assertNotEqual(sen1c[2].type, sen1[2].type)
        self.assertNotEqual(sen2c[2].type, sen2[2].type)
        self.assertNotEqual(sen2c[3].type, sen2[3].type)

    def test_type_unify_with_exclusion_basic(self):
        sen1 = self.dsl("a!test")[0]
        sen2 = self.dsl("a.test")[0]

        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(sen1, sen2, CtxIns())

    def test_type_unify_type_sen_with_exclusion(self):
        sen1 = self.dsl("a.test(::a.simple!type)")[0]
        sen2 = self.dsl("a.test(::a.simple.type)")[0]

        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(sen1, sen2, CtxIns())

    def test_type_unify_type_sen_with_var(self):
        sen1 = self.dsl("a.test(::a.simple!type)")[0]
        sen2 = self.dsl("a.test(::a.$x!type)")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertEqual(result.x, "simple")

    def test_nested_sentence_unify(self):
        sen1 = self.dsl("a.test.[[d.e.f]]")[0]
        sen2 = self.dsl("a.test.[[d.e.f]]")[0]

        result = tuf.type_unify(sen1, sen2, CtxIns())


    def test_nested_sentence_unify_var(self):
        sen1 = self.dsl("a.test.[[d.e.f]]")[0]
        sen2 = self.dsl("a.test.$x")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertEqual(result['x'], "_:d.e.f")

    def test_nested_sentence_unify_var_reverse(self):
        sen1 = self.dsl("a.test.$x")[0]
        sen2 = self.dsl("a.test.[[d.e.f]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertEqual(result['x'], "_:d.e.f")

    def test_nested_sentence_unify_internal_conflict(self):
        sen1 = self.dsl("a.test.[[d.e.g]]")[0]
        sen2 = self.dsl("a.test.[[d.e.f]]")[0]
        with self.assertRaises(TE.AcabTypingException):
            result = tuf.type_unify(sen1, sen2, CtxIns())


    def test_nested_sentence_unify_conflict(self):
        sen1 = self.dsl("a.test.d.e.f")[0]
        sen2 = self.dsl("a.test.[[d.e.f]]")[0]
        with self.assertRaises(TE.AcabTypingException):
            result = tuf.type_unify(sen1, sen2, CtxIns())

    def test_nested_sentence_unify_internal_var(self):
        sen1 = self.dsl("a.test.[[d.e.$x]]")[0]
        sen2 = self.dsl("a.test.[[d.e.f]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertEqual(result['x'], "f")

    def test_nested_sentence_unify_type(self):
        sen1 = self.dsl("a.test.[[d.e.f]](::SENTENCE.blah)")[0]
        sen2 = self.dsl("a.test.[[d.e.f]](::SENTENCE.blah)")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())

    def test_nested_sentence_unify_type_conflict(self):
        sen1 = self.dsl("a.test.[[d.e.f]](::SENTENCE.bloo)")[0]
        sen2 = self.dsl("a.test.[[d.e.f]](::SENTENCE.blah)")[0]
        with self.assertRaises(TE.AcabTypingException):
            result = tuf.type_unify(sen1, sen2, CtxIns())

    def test_nested_sentence_unify_internal_type_conflict(self):
        sen1 = self.dsl("a.test.[[d.e(::bloo).f]]")[0]
        sen2 = self.dsl("a.test.[[d.e(::blah).f]]")[0]
        with self.assertRaises(TE.AcabTypingException):
            result = tuf.type_unify(sen1, sen2, CtxIns())

    def test_nested_sentence_unify_internal_type_var(self):
        sen1 = self.dsl("a.test.[[d.e(::$x).f]]")[0]
        sen2 = self.dsl("a.test.[[d.e(::blah).f]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertEqual(result['x'], "_:blah")


    def test_multi_nested_sentence_unify(self):
        # logmod.root.setLevel(logmod.DEBUG)
        # [x.setLevel(logmod.DEBUG) for x in logmod.root.handlers]
        sen1 = self.dsl("a.test.[[ [[a.b.d]].[[e.f.g]] ]]")[0]
        sen2 = self.dsl("a.test.[[ [[a.b.d]].[[e.f.g]] ]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        # TODO verify its just default bindings of SENTENCE

    def test_multi_nested_sentence_unify_conflict(self):
        sen1 = self.dsl("a.test.[[ [[a.b.d]].[[e.f.h]] ]]")[0]
        sen2 = self.dsl("a.test.[[ [[a.b.d]].[[e.f.g]] ]]")[0]
        with self.assertRaises(TE.AcabTypingException):
            result = tuf.type_unify(sen1, sen2, CtxIns())

    def test_multi_nested_sentence_unify_var(self):
        sen1 = self.dsl("a.test.[[ [[a.b.d]].[[e.f.g]] ]]")[0]
        sen2 = self.dsl("a.test.[[ [[a.b.d]].$x ]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertEqual(result['x'], "_:e.f.g")

    def test_multi_nested_sentence_unify_internal_var(self):
        sen1 = self.dsl("a.test.[[ [[a.b.d]].[[e.f.g]] ]]")[0]
        sen2 = self.dsl("a.test.[[ [[a.b.d]].[[e.f.$x]] ]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertEqual(result['x'], "g")


    def test_multi_nested_early_bind(self):
        sen1 = self.dsl("a.b.[[ [[a.b.d]].[[e.f.g]] ]]")[0]
        sen2 = self.dsl("a.b.$x")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertEqual(result['x'], "_:[a.b.d].[e.f.g]")

    def test_nested_vars_with_bindings(self):
        sen1 = self.dsl("a.b.[[a.$x]]")[0]
        sen2 = self.dsl("a.b.[[a.$y(::blah)]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertEqual(result['x^type'], "_:blah")
        self.assertEqual(result['y^type'], "_:blah")

    def test_nested_sen_with_type_upgrade(self):
        sen1 = self.dsl("a.b.[[a.bloo]]")[0]
        sen2 = self.dsl("a.b.[[a.$y(::blah)]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1][-1].type, "_:blah")

    def test_nested_apply_type_var(self):
        sen1 = self.dsl("a.test.[[$x(::$y)]]")[0]
        ctx = CtxIns(data={'x': self.dsl("blah")[0][0], 'y': self.dsl("bloo")[0]})
        sen1c = tuf.type_unify.apply(sen1, ctx)

        self.assertEqual(sen1c, "_:a.test.[blah]")
        self.assertEqual(sen1c[-1], "_:blah")
        self.assertEqual(sen1c[-1][0].type, "_:bloo")

    def test_full_sen_bind(self):
        sen1 = self.dsl("$x")[0]
        sen2 = self.dsl("a.b.c")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c, VF.sen() << sen2)

    def test_full_sen_bind_flatten(self):
        sen1 = self.dsl("$x(♭)")[0]
        sen2 = self.dsl("a.b.c")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())

        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c, sen2)

    def test_full_sen_bind_type_preserve(self):
        sen1 = self.dsl("a.b.$x(::SENTENCE.blah)")[0]
        sen2 = self.dsl("a.b.[[a.b.c(::blah)]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen2, sen1c)

    def test_full_sen_bind_type_preserve_2(self):
        sen1 = self.dsl("[[$x(::SENTENCE.blah, ♭)]]")[0]
        sen2 = self.dsl("[[a.b.c(::blah)]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c, sen2)


    def test_sequence(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b.c(::test)")[0]
        sen3 = self.dsl("$x.$y.returns.$z")[0]
        sen4 = self.dsl("$q(::test).$w.returns.$r(::bloo)")[0]

        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test")

        result2 = tuf.type_unify(sen3, sen4, result)
        sen3c = tuf.type_unify.apply(sen3, result2)
        self.assertEqual(sen3c[0].type, "_:test")


    def test_sequence_conflict(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b.c(::test)")[0]
        sen3 = self.dsl("$x.$y.returns.$z")[0]
        sen4 = self.dsl("$q(::blah).$w.returns.$r(::bloo)")[0]

        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test")

        with self.assertRaises(TE.AcabTypingException):
            result2 = tuf.type_unify(sen3, sen4, result)


    def test_sequence_nested(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b.c(::test)")[0]
        sen3 = self.dsl("[[ [[a.q.$x]].[[$y(::other.blah)]] ]].[[returns.$z]]")[0]
        sen4 = self.dsl("[[ [[$i.$o.$q(::test)]].[[$w(::other)]] ]].[[returns.$r(::bloo)]]")[0]

        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test")
        result2 = tuf.type_unify(sen3, sen4, result)

        self.assertEqual(result2['z^type'], "_:bloo")
        self.assertEqual(result2['x^type'], "_:test")
        self.assertEqual(result2['y^type'], "_:other")

    def test_sequence_nested_subtype_conflict(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b.c(::test)")[0]
        sen3 = self.dsl("[[ [[a.q.$x]].[[$y(::not.other.blah)]] ]].[[returns.$z]]")[0]
        sen4 = self.dsl("[[ [[$i.$o.$q(::test)]].[[$w(::other)]] ]].[[returns.$r(::bloo)]]")[0]

        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test")

        with self.assertRaises(TE.AcabTypingException):
            result2 = tuf.type_unify(sen3, sen4, result)

    def test_sequence_type_var(self):
        sen1 = self.dsl("a.$y.$x")[0]
        sen2 = self.dsl("a.b(::blah).c(::test.blah)")[0]
        sen3 = self.dsl("$x.$y.returns.$z")[0]
        sen4 = self.dsl("$q(::test.$a).$w(::$a).returns.$r(::bloo)")[0]


        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test.blah")
        result2 = tuf.type_unify(sen3, sen4, result)
        sen3c = tuf.type_unify.apply(sen3, result2)
        self.assertEqual(sen3c[1].type, "_:blah")

    def test_sequence_type_var_conflict(self):
        sen1 = self.dsl("a.$y(::blah).$x")[0]
        sen2 = self.dsl("a.b(::blah).c(::test.blah)")[0]
        sen3 = self.dsl("$x.$y.returns.$z")[0]
        sen4 = self.dsl("$q(::test.$a).$w(::bloo).returns.$r(::bloo)")[0]


        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test.blah")
        with self.assertRaises(TE.AcabTypingException):
            result2 = tuf.type_unify(sen3, sen4, result)

    def test_sequence_type_var_2(self):
        sen1 = self.dsl("a.$y(::blah).$x")[0]
        sen2 = self.dsl("a.b(::$b).c(::test.blah)")[0]
        sen3 = self.dsl("$x.$y.returns.$z")[0]
        sen4 = self.dsl("$q(::test.$a).$w(::$a).returns.$r(::bloo)")[0]


        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test.blah")
        result2 = tuf.type_unify(sen3, sen4, result)


        self.assertEqual(result2.y, "b")
        self.assertEqual(result2['y^type'], "_:blah")
        self.assertEqual(result2.x, "c")
        self.assertEqual(result2['x^type'], "_:test.blah")
        self.assertEqual(result2.a, "_:blah")
        self.assertEqual(result2.w, "b")
        self.assertEqual(result2['w^type'], "_:blah")
        self.assertEqual(result2.r, "z")
        self.assertEqual(result2['z^type'], "_:bloo")
        self.assertEqual(result2['r^type'], "_:bloo")


    def test_sequence_type_var_conflict_2_resolution(self):
        sen1 = self.dsl("a.$y(::blah).$x")[0]
        sen2 = self.dsl("a.b(::$b).c(::test.blah)")[0]
        sen3 = self.dsl("$x.$y.returns.$z")[0]
        sen4 = self.dsl("$q(::test.$a).$w(::$a).returns.$r(::bloo)")[0]


        result = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, result)
        self.assertEqual(sen1c[-1].type, "_:test.blah")
        result2 = tuf.type_unify(sen3, sen4, result)

    def test_apply_types_onto_sen(self):
        sen1 = self.dsl("a.$y(::blah).$x(::$g)")[0]
        sen2 = self.dsl("a.b(::$b).c(::test.blah)")[0]
        sen3 = self.dsl("$x.$y.returns.$z")[0]
        sen4 = self.dsl("$q(::test.$a).$w(::$a).returns.$r(::bloo)")[0]

        result = tuf.type_unify(sen1, sen2, CtxIns())
        result2 = tuf.type_unify(sen3, sen4, result)

        sen1d = tuf.apply_types_onto_sen(sen1, result2)
        self.assertEqual(sen1d, "_:a.y.x")
        self.assertEqual(sen1d[1].type, "_:blah")
        self.assertEqual(sen1d[-1].type, "_:test.blah")

        sen2d = tuf.apply_types_onto_sen(sen2, result2)
        self.assertEqual(sen2d, "_:a.b.c")
        self.assertEqual(sen2d[1].type, "_:blah")
        self.assertEqual(sen2d[-1].type, "_:test.blah")

        sen3d = tuf.apply_types_onto_sen(sen3, result2)
        self.assertEqual(sen3d, "_:x.y.returns.z")
        self.assertEqual(sen3d[0].type, "_:test.blah")
        self.assertEqual(sen3d[1].type, "_:blah")
        self.assertEqual(sen3d[-1].type, "_:bloo")

        sen4d = tuf.apply_types_onto_sen(sen4, result2)
        self.assertEqual(sen4d, "_:q.w.returns.r")
        self.assertEqual(sen4d[0].type, "_:test.blah")
        self.assertEqual(sen4d[1].type, "_:blah")
        self.assertEqual(sen4d[-1].type, "_:bloo")


        # $x -> a
        # $x -> a.b.c
        # [$x] -> [a]
        # [$x] -> [a.b.c]
        # [[$x]] -> [[a]]
        # [[$x]] -> [[a.b.c]]
        # a.b.$x -> a.b.c
        # a.b.$x -> a.b.[d.e.f]

        # (params:[ 1:[$a].2:[a.b.$x] ]).[returns.$y]


    def test_nested_double_var(self):
        sen1 = self.dsl("[[$x]]")[0]
        sen2 = self.dsl("[[$y(::blah)]]")[0]
        result = tuf.type_unify(sen1, sen2, CtxIns())

        sen1c = tuf.apply_types_onto_sen(sen1, result)

        self.assertEqual(sen1c, sen1)
        self.assertEqual(sen1c[0][0].type, "_:blah")
    def test_minimal_var_chain(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b.$y")[0]
        sen3 = self.dsl("$x.d.f")[0]
        sen4 = self.dsl("$z.d.f")[0]
        sen5 = self.dsl("q.$x.g")[0]
        sen6 = self.dsl("$q.$b.$m")[0]

        res1 = tuf.type_unify(sen1, sen2, CtxIns())
        res2 = tuf.type_unify(sen3, sen4, res1)
        res3 = tuf.type_unify(sen5, sen6, res2)
        self.assertEqual(res3.y, "x")
        self.assertEqual(res3.z, "x")
        self.assertEqual(res3.b, "x")


    def test_var_boundary_crossing(self):
        pass
