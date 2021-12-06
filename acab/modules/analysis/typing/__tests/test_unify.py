import logging as root_logger
import unittest
import unittest.mock as mock
from functools import partial
from os.path import split, splitext

from acab import setup

config = setup()

import acab.modules.analysis.typing.simple_unify_fns as suf
import acab.modules.analysis.typing.type_unify_fns as tuf
from acab.core.data.acab_struct import AcabNode
from acab.core.data.default_structure import BIND
from acab.core.data.values import AcabValue, Sentence
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.analysis.typing import type_exceptions as TE
from acab.modules.analysis.typing import unify
from acab.modules.analysis.typing.typing_dsl import TypingDSL
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.context.context_set import MutableContextInstance
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser

# Set up the parser to ease test setup
dsl   = ppDSL.PyParseDSL()
dsl.register(EXLO_Parser).register(TypingDSL)
dsl.build()

class BasicUnifyTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        root_logger.getLogger('').setLevel(root_logger.WARNING)
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])

        file_h = root_logger.FileHandler(LOG_FILE_NAME, mode='w')
        file_h.setLevel(root_logger.DEBUG)

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.WARNING)

        logging = root_logger.getLogger(__name__)
        logging.setLevel(root_logger.DEBUG)
        logging.addHandler(console)
        logging.addHandler(file_h)


    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    # use testcase snippet
    # mock.Mock / MagicMock
    # create_autospec
    # @patch(' ') / with patch.object(...)


    def test_basic(self):
        sen1 = dsl.parseString("a.test.sentence")[0]
        sen2 = dsl.parseString("a.test.sentence")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertFalse(ctx_r)


    def test_variable(self):
        sen1 = dsl.parseString("a.test.sentence")[0]
        sen2 = dsl.parseString("a.test.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 1)
        self.assertIn('x', ctx_r)
        self.assertEqual(ctx_r['x'], "sentence")
        self.assertIsInstance(ctx_r['x'], AcabValue)

    def test_variable_with_gamma(self):
        gamma = CtxIns({ 'x' : dsl.parseString("blah.bloo")[0]})
        sen1 = dsl.parseString("a.test.$x")[0]
        sen2 = dsl.parseString("a.test.$y")[0]

        ctx_r = suf.basic_unify(sen1, sen2, gamma)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 2)
        self.assertIn('x', ctx_r)
        self.assertEqual(ctx_r['x'], "_:blah.bloo")
        self.assertIsInstance(ctx_r['x'], AcabValue)
        self.assertIn('y', ctx_r)
        self.assertEqual(ctx_r[ctx_r['y']], "_:blah.bloo")

    def test_variable_chain_minimisation(self):
        gamma = CtxIns({ 'x' : dsl.parseString("blah.bloo")[0]})
        sen1 = dsl.parseString("a.test.$x")[0]
        sen2 = dsl.parseString("a.test.$y")[0]
        sen3 = dsl.parseString("a.test.$z")[0]

        ctx_r = suf.basic_unify(sen1, sen2, gamma)
        ctx_r = suf.basic_unify(sen2, sen3, ctx_r)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 3)
        self.assertEqual(ctx_r.x, dsl.parseString("blah.bloo")[0])
        self.assertEqual(ctx_r.y, "x")
        self.assertEqual(ctx_r.z, "y")

    def test_variable_chain_minimisation_2(self):
        gamma = CtxIns({ 'x' : dsl.parseString("blah.bloo")[0]})
        sen1 = dsl.parseString("a.test.$x.$z")[0]
        sen2 = dsl.parseString("a.test.$y.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, gamma)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 3)
        self.assertEqual(ctx_r.y, "x")
        self.assertEqual(ctx_r.z, "x")

    def test_unify_then_bind(self):
        sen1 = dsl.parseString("a.test.sentence")[0]
        sen2 = dsl.parseString("a.test.$x")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        sen3 = suf.basic_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1, sen3)


    def test_unify_then_bind2(self):
        total = dsl.parseString("a.test.sentence")[0]
        sen1  = dsl.parseString("a.$y.sentence")[0]
        sen2  = dsl.parseString("a.test.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        sen3  = suf.basic_sen_logic.apply(sen1, ctx_r)

        self.assertEqual(total, sen3)


    def test_unify_fail(self):
        sen1  = dsl.parseString("a.$x.sentence")[0]
        sen2  = dsl.parseString("a.test.$x")[0]

        with self.assertRaises(TE.AcabTypingException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())


    def test_unify_duplicate(self):
        sen1  = dsl.parseString("a.$x.test")[0]
        sen2  = dsl.parseString("a.test.$x")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertTrue('x' in ctx_r)
        self.assertEqual(ctx_r.x, "test")

    def test_unify_vars(self):
        sen1  = dsl.parseString("a.test.$x")[0]
        sen2  = dsl.parseString("a.test.$y")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        sen3  = suf.basic_sen_logic.apply(sen1, ctx_r)

        self.assertTrue('x' in ctx_r)
        self.assertEqual(sen2, sen3)

    def test_unify_conflict(self):
        sen1  = dsl.parseString("a.test.$x.bloo")[0]
        sen2  = dsl.parseString("a.test.blah.$x")[0]

        with self.assertRaises(TE.TypeUnifyException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

    def test_unify_chain(self):
        sen1  = dsl.parseString("a.test.$x.$y")[0]
        sen2  = dsl.parseString("a.test.$y.bloo")[0]

        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertIn("x", ctx_r)
        self.assertIn("y", ctx_r)
        self.assertEqual(ctx_r.x, "y")
        self.assertEqual(ctx_r.y, "bloo")


    def test_unify_fail(self):
        sen1 = dsl.parseString("a.b.c")[0]
        sen2 = dsl.parseString("d.b.c")[0]

        with self.assertRaises(TE.TypeUnifyException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())


    def test_len_diff_true_sub(self):
        sen1 = dsl.parseString("a.b.c")[0]
        sen2 = dsl.parseString("a.b")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertFalse(ctx_r)

    def test_right_var(self):
        sen1 = dsl.parseString("a.b.c")[0]
        sen2 = dsl.parseString("a.b.$x")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "c")


    def test_left_var(self):
        sen1 = dsl.parseString("a.b.$x")[0]
        sen2 = dsl.parseString("a.b")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertNotIn("x", ctx_r)

    def test_left_var_crit_path(self):
        sen1 = dsl.parseString("a.$x.c")[0]
        sen2 = dsl.parseString("a.b.c")[0]
        ctx_r = suf.basic_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "b")


