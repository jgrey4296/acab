#https://docs.python.org/3/library/unittest.html
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
    import pyparsing as pp
    from acab.core.parsing import pyparse_dsl as ppDSL
    # if '@pytest_ar' in globals():
    #     from acab.core.parsing import debug_funcs as DBF
    #     DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    from acab.core.defaults import value_keys as DS
    from acab.core.parsing.component_dsl import Component_DSL
    from acab.core.value.value import AcabValue
    from acab.interfaces import value as VI
    from acab.modules.analysis.typing.module import TypeSpecFragment
    from acab.modules.context.context_set import ContextInstance as CtxIns
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.modules.values.sen_val.module import Sen_Val_Parser

    from ... import exceptions as TE
    from .. import simple_unify_fns as suf
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
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(TypeSpecFragment().build_dsl())
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
    def test_basic(self):
        sen1 = self.dsl("a.test.sentence")[0]
        sen2 = self.dsl("a.test.sentence")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertFalse(ctx_r)

    def test_variable(self):
        sen1 = self.dsl("a.test.sentence")[0]
        sen2 = self.dsl("a.test.$x")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 1)
        self.assertIn('x', ctx_r)
        self.assertEqual(ctx_r['x'], "sentence")
        self.assertIsInstance(ctx_r['x'], AcabValue)

    def test_variable_with_gamma(self):
        gamma = CtxIns({ 'x' : self.dsl("blah.bloo")[0]})
        sen1 = self.dsl("a.test.$x")[0]
        sen2 = self.dsl("a.test.$y")[0]

        ctx_r = suf.basic_unify(sen1, sen2, gamma)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 2)
        self.assertIn('x', ctx_r)
        self.assertEqual(ctx_r['x'], "_:blah.bloo")
        self.assertIsInstance(ctx_r['x'], VI.Value_i)
        self.assertIn('y', ctx_r)
        self.assertEqual(ctx_r[ctx_r['y']], "_:blah.bloo")

    def test_variable_chain_minimisation(self):
        gamma = CtxIns({ 'x' : self.dsl("blah.bloo")[0]})
        sen1 = self.dsl("a.test.$x")[0]
        sen2 = self.dsl("a.test.$y")[0]
        sen3 = self.dsl("a.test.$z")[0]

        ctx_r = suf.basic_unify(sen1, sen2, gamma)
        ctx_r = suf.basic_unify(sen2, sen3, ctx_r)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 3)
        self.assertEqual(ctx_r.x, self.dsl("blah.bloo")[0])
        self.assertEqual(ctx_r.y, "x")
        self.assertEqual(ctx_r.z, "y")

    def test_variable_chain_minimisation_2(self):
        gamma = CtxIns({ 'x' : self.dsl("blah.bloo")[0]})
        sen1 = self.dsl("a.test.$x.$z")[0]
        sen2 = self.dsl("a.test.$y.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, gamma)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 3)
        self.assertEqual(ctx_r.y, "x")
        self.assertEqual(ctx_r.z, "x")

    def test_unify_then_bind(self):
        sen1 = self.dsl("a.test.sentence")[0]
        sen2 = self.dsl("a.test.$x")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        sen3 = suf.basic_unify.apply(sen2, ctx_r)

        self.assertEqual(sen1, sen3)


    def test_unify_then_bind2(self):
        total = self.dsl("a.test.sentence")[0]
        sen1  = self.dsl("a.$y.sentence")[0]
        sen2  = self.dsl("a.test.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        sen3  = suf.basic_unify.apply(sen1, ctx_r)

        self.assertEqual(total, sen3)


    def test_unify_fail(self):
        sen1  = self.dsl("a.$x.sentence")[0]
        sen2  = self.dsl("a.test.$x")[0]

        with self.assertRaises(TE.AcabTypingException):
            suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_fail_2(self):
        sen1  = self.dsl("a.blah.sentence")[0]
        sen2  = self.dsl("a.test.sentence")[0]

        with self.assertRaises(TE.AcabTypingException):
            suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_duplicate(self):
        sen1  = self.dsl("a.$x.test")[0]
        sen2  = self.dsl("a.test.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertTrue('x' in ctx_r)
        self.assertEqual(ctx_r.x, "test")

    def test_unify_vars(self):
        sen1  = self.dsl("a.test.$x")[0]
        sen2  = self.dsl("a.test.$y")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        sen3  = suf.basic_unify.apply(sen1, ctx_r)

        self.assertTrue('x' in ctx_r)
        self.assertEqual(sen2, sen3)

    def test_unify_conflict(self):
        sen1  = self.dsl("a.test.$x.bloo")[0]
        sen2  = self.dsl("a.test.blah.$x")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_conflict_same_side(self):
        sen1  = self.dsl("a.test.$x.$x")[0]
        sen2  = self.dsl("a.test.blah.bloo")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())


    def test_unify_chain(self):
        sen1  = self.dsl("a.test.$x.$y")[0]
        sen2  = self.dsl("a.test.$y.bloo")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertIn("x", ctx_r)
        self.assertIn("y", ctx_r)
        self.assertEqual(ctx_r.x, "y")
        self.assertEqual(ctx_r.y, "bloo")


    def test_unify_fail(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("d.b.c")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())


    def test_len_diff_true_sub(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("a.b")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertFalse(ctx_r)

    def test_right_var(self):
        sen1 = self.dsl("a.b.c")[0]
        sen2 = self.dsl("a.b.$x")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "c")


    def test_left_var(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertNotIn("x", ctx_r)

    def test_left_var_crit_path(self):
        sen1 = self.dsl("a.$x.c")[0]
        sen2 = self.dsl("a.b.c")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "b")


    def test_simple_chain(self):
        sen1   = self.dsl("a.test.sentence")[0]
        sen2   = self.dsl("a.test.$x")[0]
        sen3   = self.dsl("a.$y.sentence")[0]
        ctx_r  = suf.basic_unify(sen1, sen2, CtxIns())
        ctx_r2 = suf.basic_unify(sen2, sen3, ctx_r)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 1)
        self.assertIn('x', ctx_r)
        self.assertEqual(ctx_r['x'], "sentence")
        self.assertIsInstance(ctx_r['x'], AcabValue)

    def test_unify_exclusion(self):
        sen1 = self.dsl("a.test")[0]
        sen2 = self.dsl("a!test")[0]

        with self.assertRaises(TE.AcabTypingException):
            suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_exclusion2(self):
        sen1 = self.dsl("a.test.sentence")[0]
        sen2 = self.dsl("a.test!sentence")[0]

        with self.assertRaises(TE.AcabTypingException):
            suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_exclusion_var(self):
        sen1 = self.dsl("a.$x.sentence")[0]
        sen2 = self.dsl("a.test!sentence")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        sen3 = suf.basic_unify.apply(sen1, ctx_r)
        sen4 = self.dsl("a.test.sentence")[0]

        with self.assertRaises(TE.AcabTypingException):
            suf.basic_unify(sen3, sen4, CtxIns())


    def test_unify_exclusion_missing(self):
        """
        Ensure modal check can handle when a word is missing
        the modality because its the terminal of the sentence
        """
        sen1 = self.dsl("a.test.sen!blah")[0]
        sen2 = self.dsl("a.test.sen")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertTrue(not bool(ctx_r))

    def test_unify_ignore_query(self):
        """
        Test whether a query terminal is significant
        (currently not)
        """
        sen1 = self.dsl("a.$x.sentence")[0]
        sen2 = self.dsl("a.test!sentence?")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertEqual(ctx_r.x, "test")

    def test_unify_nested_conflict(self):
        sen1 = self.dsl("a.b")[0] << (VI.ValueFactory.sen() << ["d","e","f"])
        sen2 = self.dsl("a.b")[0] << (VI.ValueFactory.sen() << ["d","e","g"])

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_nested(self):
        sen1 = self.dsl("a.b")[0] << (VI.ValueFactory.sen() << ["d","e","f"])
        sen2 = self.dsl("a.b")[0] << (VI.ValueFactory.sen() << ["d","e","f"])

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertFalse(ctx_r)

    def test_unify_nested_var(self):
        sen1 = self.dsl("a.b")[0] << (VI.ValueFactory.sen() << ["d","e","f"])
        sen2 = self.dsl("a.b")[0] << (VI.ValueFactory.sen() << ["d","e","x"])

        sen2[-1][-1].data[DS.BIND] = True
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertIn("x", ctx_r)
        self.assertEqual(ctx_r.x, "f")

    def test_unify_var_against_nested(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b")[0] << self.dsl("d.e.f")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertEqual(ctx_r.x, "_:d.e.f")

    def test_unify_nested_sentences(self):
        sen1 = self.dsl("a.b.[[c.d.ef]]")[0]
        sen2 = self.dsl("a.b.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertEqual(ctx_r['x'], "_:c.d.ef")

    def test_unify_nested_sentences_invert(self):
        sen1 = self.dsl("a.b.$x")[0]
        sen2 = self.dsl("a.b.[[c.d.ef]]")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertEqual(ctx_r['x'], "_:c.d.ef")

    def test_unify_nested_sentences_sub_check(self):
        sen1 = self.dsl("a.b.[[c.d.$x]]")[0]
        sen2 = self.dsl("a.b.[[c.d.ef]]")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertEqual(ctx_r['x'], "ef")

    def test_unify_nested_sentences_sub_check_sharp(self):
        sen1 = self.dsl("a.b.♯[[c.d.$x]]")[0]
        sen2 = self.dsl("a.b.♯[[c.d.ef]]")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertEqual(ctx_r['x'], "ef")

    def test_unify_nested_sentences_check_sharp(self):
        sen1 = self.dsl("♯a.b.[[c.d.$x]]")[0]
        sen2 = self.dsl("♯a.b.[[c.d.ef]]")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertEqual(ctx_r['x'], "ef")

    def test_unify_nested_sentences_fail_internal(self):
        sen1 = self.dsl("♯a.b.[[c.d.g]]")[0]
        sen2 = self.dsl("♯a.b.[[c.d.ef]]")[0]
        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_nested_sentences_surface(self):
        sen1 = self.dsl("♯a.b.c.d.ef")[0]
        sen2 = self.dsl("♯a.b.[[c.d.ef]]")[0]
        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
