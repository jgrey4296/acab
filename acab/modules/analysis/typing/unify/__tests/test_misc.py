#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging as root_logger

from acab import setup
config = setup()

from acab.core.data.value import AcabValue

from ... import exceptions as TE
from .. import unify
from .. import simple_unify_fns as suf
from .. import type_unify_fns as tuf
from .. import util

from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.analysis.typing.dsl import TypingDSL
from acab.modules.context.context_set import ContextInstance as CtxIns

dsl   = ppDSL.PyParseDSL()
dsl.register(EXLO_Parser).register(TypingDSL)
dsl.build()


class UnifierTests(unittest.TestCase):

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


    def test_unify_types(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.sentence(::blah)")[0]
        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertTrue(ctx_r)
        self.assertIn(id(sen1[-1].type), ctx_r)
        self.assertEqual(ctx_r[id(sen1[-1].type)], dsl("blah")[0])

    def test_apply_types_var(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.$x(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(ctx_r[sen1c[-1]].type, sen2[-1].type)

    def test_apply_types_var_left(self):
        sen1 = dsl("a.test.$x")[0]
        sen2 = dsl("a.test.sentence(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-1].type)

    def test_apply_types_var_left_repeated(self):
        sen1 = dsl("a.test.$x.$x")[0]
        sen2 = dsl("a.test.sentence(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-2].type)

    def test_apply_types_var_left_repeated_conflict(self):
        sen1 = dsl("a.test.$x.$x(::bloo)")[0]
        sen2 = dsl("a.test.sentence(::blah).awf(::$y)")[0]

        with self.assertRaises(TE.TypeConflictException):
            ctx_r = tuf.type_unify(sen1, sen2, CtxIns())



    def test_apply_type_var(self):
        sen1 = dsl("a.test.$x(::$y)")[0]
        sen2 = dsl("a.test.sentence(::blah.bloo)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertEqual(sen1c[-1].type, dsl("blah.bloo")[0])



    def test_apply_types_generalise(self):
        sen1 = dsl("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = dsl("a.test(::blah).sentence(::a.b)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-2].type, "_:blah")
        self.assertEqual(sen1c[-1].type, "_:a.b")

    def test_apply_types_with_vars(self):
        sen1 = dsl("a.test.sentence")[0]
        sen2 = dsl("a.test.$x(::blah!$y)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-1].type, "_:blah.y")
        self.assertTrue(sen1c[-1].type[-1].is_var)
        self.assertIn("y", ctx_r)
        self.assertIn(id(sen1[-1].type), ctx_r)

    def test_apply_types_with_vars_completed(self):
        sen1 = dsl("a.test.sentence.bloo(::aweg.awg)")[0]
        sen2 = dsl("a.test.$x(::blah!$y).$z(::$y)")[0]


        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-1].type, "_:aweg.awg")
        self.assertEqual(str(sen1c[-2].type), "blah.aweg.awg")
        self.assertEqual(sen1c[-2].type[-1], "_:aweg.awg")
        self.assertEqual(sen2c[-1].type, "_:aweg.awg")
        self.assertEqual(sen2c[-2].type, "_:blah.aweg.awg")
        self.assertEqual(sen2c[-2].type[-1], "_:aweg.awg")
        self.assertNotEqual(sen1c, sen2c)
        self.assertNotEqual(sen1c[-2].type, "_:blah.y")
