#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

import logging as logmod
import unittest
import unittest.mock as mock
import warnings
from os.path import split, splitext

from acab import setup

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = setup()

from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.parsing.annotation import ValueAnnotation
from acab.core.value import default_structure as DS
from acab.core.value.value import AcabValue
from acab.modules.analysis.typing.module import TypingDSL
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser

from ... import exceptions as TE
from .. import simple_unify_fns as suf
from .. import type_unify_fns as tuf
from .. import unifier as unify
from .. import util

dsl = None

gen_f = util.gen_f

# AcabReducible          : type_definition -> sentences with unique variable at head
# Sentence.remove_prefix : Sentence remove prefix prior to type being checked
# Typing is a statement. use Query : Type retrieval from WM
# Use Rules for : Product / Sum / Operator Type differentiation

class UnifierTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        global dsl
        dsl   = ppDSL.PyParseDSL()
        dsl.register(EXLO_Parser).register(TypingDSL)
        dsl.build()

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------


    def test_unify_types(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.sentence(::blah)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        self.assertTrue(ctx_r)
        self.assertEqual(ctx_r[str(sen1)], ValueAnnotation(DS.TYPE_INSTANCE,
                                                           dsl("blah")[0]))
        self.assertEqual(sen1[-1].type, "_:ATOM")

    def test_types_check_simple(self):
        sen1 = dsl("a(::blah).test.sentence")[0]
        sen2 = dsl("$x(::blah).test.sentence(::bloo)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertTrue(ctx_r)
        self.assertIn(str(sen1[:]), ctx_r)
        self.assertEqual(ctx_r[str(sen1[:])], ValueAnnotation(DS.TYPE_INSTANCE,
                                                              dsl("bloo")[0]))

    def test_check_simple_fail(self):
        sen1 = dsl("a(::blah).blah.bloo")[0]
        sen2 = dsl("$x(::blah).test.sentence")[0]

        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify(sen1, sen2, CtxIns())



    def test_types_check_simple_fail(self):
        sen1 = dsl("a(::blah).test.sentence(::aweg)")[0]
        sen2 = dsl("$x(::blah).test.sentence(::bloo)")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = tuf.type_unify(sen1, sen2, CtxIns())


    def test_apply_types_generalise(self):
        sen1 = dsl("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = dsl("a.test(::blah).sentence(::a.b)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertIn("a.test", ctx_r)
        self.assertIn("a.test.sentence", ctx_r)
        self.assertIsInstance(ctx_r['a.test'], ValueAnnotation)
        self.assertIsInstance(ctx_r['a.test.sentence'], ValueAnnotation)
        self.assertEqual(ctx_r['a.test'], ValueAnnotation(DS.TYPE_INSTANCE,
                                                          dsl("blah")[0]))
        self.assertEqual(ctx_r['a.test.sentence'], ValueAnnotation(DS.TYPE_INSTANCE,
                                                                   dsl("a.b")[0]))

    def test_apply_types_with_vars(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.$x(::blah!$y)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-1].type, "_:blah.y")
        self.assertTrue(sen1c[-1].type[-1].is_var)
        self.assertIn("y", ctx_r)
        self.assertIn(str(sen1), ctx_r)

    def test_apply_types_with_vars_completed(self):
        sen1 = dsl("a.test.sentence.bloo(::aweg.awg)")[0]
        sen2 = dsl("a.test.$x(::blah!$y).$z(::$y)")[0]


        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)
        self.assertEqual(sen1c[-1].type,      "_:aweg.awg")
        self.assertEqual(str(sen1c[-2].type), "blah.aweg.awg")
        self.assertEqual(sen1c[-2].type[-1],  "_:aweg.awg")

        self.assertEqual(sen2c[-1].type,      "_:aweg.awg")
        self.assertEqual(sen2c[-2].type,      "_:blah.aweg.awg")
        self.assertEqual(sen2c[-2].type[-1],  "_:aweg.awg")

        self.assertNotEqual(sen1c[-2].type,   "_:blah.y")


    def test_repeat_unify_one_to_many(self):
        a_sen    = dsl("a.test.sen(::def).sub.blah")[0]
        # remove initial prefix
        chopped  = a_sen.remove_prefix(dsl("a.test")[0])
        type_    = dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_.to_sentences()
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        unified  = tuf.type_unify.repeat([chopped], appended, CtxIns())
        result   = tuf.type_unify.apply(appended[0], unified)

        self.assertEqual(chopped, result)

    def test_repeat_unify_one_to_many_fail(self):
        a_sen    = dsl("a.test.sen(::def).awef.blah")[0]
        # remove initial prefix
        chopped  = a_sen.remove_prefix(dsl("a.test")[0])
        type_    = dsl("def(::σ):\n sub.$x(::test)\n other.$y(::blah)\nend")[0][-1]
        as_sens  = type_.to_sentences()
        new_var  = gen_f()
        # Add unique var prefix
        appended = [new_var.add(x) for x in as_sens]
        # unify them:
        with self.assertRaises(TE.AcabTypingException):
            tuf.type_unify.repeat([chopped], appended, CtxIns())

    def test_repeat_unify_many_to_many(self):
        # NOTE: sentences will always have same head
        a_sen    = dsl("sen(::def).sub.blah")[0]
        b_sen    = dsl("sen(::def).seb.bloo")[0]
        type_    = dsl("def(::σ):\n sub.$x(::test)\n seb.$y\nend")[0][-1]
        as_sens  = type_.to_sentences()
        new_var1 = gen_f()
        # Add unique var prefix
        appended = [new_var1.add(x) for x in as_sens]
        # unify them:
        unified  = tuf.type_unify.repeat([a_sen, b_sen], appended, CtxIns())

        self.assertTrue(unified)

    def test_unify_with_prior_ctx(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.$x(::blah!$y)")[0]

        ctx = CtxIns(data={"x": dsl("sentence(::blah)")[0][0]})
        ctx_r = tuf.type_unify(sen1, sen2, ctx)
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)
        self.assertEqual(sen1c[-1].type, "_:blah")
        self.assertIn("y", ctx_r)
        self.assertIn(str(sen1), ctx_r)
