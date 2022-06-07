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
    config = acab.setup()
    from acab.core.parsing import pyparse_dsl as ppDSL

from acab.interfaces import value as VI
import acab.core.defaults.value_keys as DS
from acab.core.parsing.annotation import ValueAnnotation
from acab.core.value.value import AcabValue
from acab.modules.analysis.typing.module import TypingFragment
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser

from ... import exceptions as TE
from .. import simple_unify_fns as suf
from .. import type_unify_fns as tuf
from .. import unifier as unify
from .. import util

dsl = None

class UnifierTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

        global dsl
        dsl   = ppDSL.PyParseDSL()
        dsl.register(EXLO_Parser).register(TypingFragment().build_dsl())
        dsl.build()

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------

    def test_subtype_relation_fail(self):
        sen1 = dsl("a.b.c")[0]
        sen2 = dsl("d.b.c")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

    def test_subtype_relation_true_sub(self):
        sen1 = dsl("a.b.c")[0]
        sen2 = dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

    def test_subtype_relation_true_sub_reversed(self):
        sen1 = dsl("a.b.c")[0]
        sen2 = dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen2, sen1, CtxIns())

    def test_subtype_relation_right_var(self):
        sen1 = dsl("a.b.c")[0]
        sen2 = dsl("a.b.$x")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "c")


    def test_subtype_relation_left_var(self):
        sen1 = dsl("a.b.$x")[0]
        sen2 = dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertIn("x", ctx_r)

    def test_subtype_relation_left_var_crit_path(self):
        sen1 = dsl("a.$x.c")[0]
        sen2 = dsl("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "b")


    def test_unify_types(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.sentence(::blah)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertTrue(ctx_r)
        self.assertIn(str(sen1[:]), ctx_r)
        self.assertEqual(ctx_r[str(sen1[:])], ValueAnnotation(DS.TYPE_INSTANCE,
                                                              dsl("blah")[0]))

    def test_unify_types_var(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.$x(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(ctx_r[sen1c[-1]].type, sen2[-1].type)
        self.assertEqual(sen1c[-1].type, "_:blah")

    def test_apply_types_var_left(self):
        sen1 = dsl("a.test.$x")[0]
        sen2 = dsl("a.test.sentence(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-1].type)

    def test_apply_types_var_left_repeated(self):
        sen1 = dsl("a.test.$x.$x")[0]
        sen2 = dsl("a.test.sentence(::blah)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-1].type)

    def test_types_var_left_repeated_conflict(self):
        sen1 = dsl("a.test.$x.$x(::bloo)")[0]
        sen2 = dsl("a.test.sentence(::blah).awf(::$y)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

    def test_types_var_right_repeated_conflict(self):
        sen1 = dsl("a.test.$x.$x(::bloo)")[0]
        sen2 = dsl("a.test.sentence(::blah).awf(::$y)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen2, sen1, CtxIns())


    def test_types_repeated_conflict(self):
        sen1 = dsl("a.test.val(::$x).val(::$x)")[0]
        sen2 = dsl("a.test.$x(::blah).$y(::bloo)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen2, sen1, CtxIns())

    def test_types_repeated_conflict(self):
        sen1 = dsl("a.test.val(::$x).val(::$x)")[0]
        sen2 = dsl("a.test.val(::blah).val(::bloo)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen2, sen1, CtxIns())


    def test_apply_type_var(self):
        sen1 = dsl("a.test.$x(::$y)")[0]
        sen2 = dsl("a.test.sentence(::blah.bloo)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertEqual(len(ctx_r), 2)
        self.assertIn("x", ctx_r)
        self.assertIn("y", ctx_r)


        sen1c = tuf.type_unify.apply(sen1, ctx_r)

        self.assertEqual(sen1c[-1].type, dsl("blah.bloo")[0])



    def test_apply_types_generalise(self):
        sen1 = dsl("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = dsl("a.test(::blah).sentence(::a.b)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        sen2c = tuf.type_unify.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-2].type, "_:blah")
        self.assertEqual(sen1c[-1].type, "_:a.b")

    def test_apply_types_with_vars(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.$x(::blah!$y)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        sen2c = tuf.type_unify.apply(sen2, ctx_r)
        self.assertEqual(sen1c[-1].type, "_:blah.y")
        self.assertTrue(sen1c[-1].type[-1].is_var)
        self.assertIn("y", ctx_r)
        self.assertEqual(ctx_r.y, "_:ATOM")
        self.assertIn(str(sen1[:]), ctx_r)
        self.assertIsInstance(ctx_r[str(sen1[:])], ValueAnnotation)
        self.assertEqual(ctx_r[str(sen1[:])], ValueAnnotation(DS.TYPE_INSTANCE,
                                                              dsl("blah!$y")[0]))

    def test_apply_types_with_vars_completed(self):
        sen1 = dsl("a.test.sentence.bloo(::aweg.awg)")[0]
        sen2 = dsl("a.test.$x(::blah!$y).$z(::$y)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.type_unify.apply(sen1, ctx_r)
        sen2c = tuf.type_unify.apply(sen2, ctx_r)

        self.assertEqual(ctx_r.x, "sentence")
        self.assertEqual(ctx_r.y, "_:aweg.awg")
        self.assertEqual(sen1c[-2], "sentence")
        self.assertEqual(sen1c[-1].type, "_:aweg.awg")
        self.assertEqual(sen1c[-2].type, "_:blah.aweg.awg")
        self.assertEqual(sen1c[-2].type[-1], "_:aweg.awg")
        self.assertEqual(sen2c[-1].type, "_:aweg.awg")
        self.assertEqual(sen2c[-2].type, "_:blah.aweg.awg")
        self.assertEqual(sen2c[-2].type[-1], "_:aweg.awg")

        self.assertNotEqual(sen1c[2].type, sen1[2].type)
        self.assertNotEqual(sen2c[2].type, sen2[2].type)
        self.assertNotEqual(sen2c[3].type, sen2[3].type)

    def test_type_unify_with_exclusion_basic(self):
        sen1 = dsl("a!test")[0]
        sen2 = dsl("a.test")[0]

        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(sen1, sen2, CtxIns())

    def test_type_unify_type_sen_with_exclusion(self):
        sen1 = dsl("a.test(::a.simple!type)")[0]
        sen2 = dsl("a.test(::a.simple.type)")[0]

        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(sen1, sen2, CtxIns())

    def test_type_unify_type_sen_with_var(self):
        sen1 = dsl("a.test(::a.simple!type)")[0]
        sen2 = dsl("a.test(::a.$x!type)")[0]

        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(sen1, sen2, CtxIns())
