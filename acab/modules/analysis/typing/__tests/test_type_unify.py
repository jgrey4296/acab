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
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.data.value import AcabValue, Sentence
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

class TypeUnifyTests(unittest.TestCase):

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


    def test_subtype_relation_fail(self):
        sen1 = dsl.parseString("a.b.c")[0]
        sen2 = dsl.parseString("d.b.c")[0]

        with self.assertRaises(TE.TypeUnifyException):
            ctx_r = suf.basic_unify(sen1, sen2, CtxIns())



    def test_subtype_relation_true_sub(self):
        sen1 = dsl.parseString("a.b.c")[0]
        sen2 = dsl.parseString("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())


    def test_subtype_relation_right_var(self):
        sen1 = dsl.parseString("a.b.c")[0]
        sen2 = dsl.parseString("a.b.$x")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "c")


    def test_subtype_relation_left_var(self):
        sen1 = dsl.parseString("a.b.$x")[0]
        sen2 = dsl.parseString("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertIn("x", ctx_r)

    def test_subtype_relation_left_var_crit_path(self):
        sen1 = dsl.parseString("a.$x.c")[0]
        sen2 = dsl.parseString("a.b")[0]
        ctx_r = tuf.type_sen_unify(sen1, sen2, CtxIns())

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "b")


    def test_unify_types(self):
        sen1 = dsl.parseString("a.test.sentence")[0]
        sen2 = dsl.parseString("a.test.sentence(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        self.assertTrue(ctx_r)
        self.assertIn(id(sen1[-1].type), ctx_r)
        self.assertEqual(ctx_r[id(sen1[-1].type)], dsl.parseString("blah")[0])

    def test_apply_types_var(self):
        sen1 = dsl.parseString("a.test.sentence")[0]
        sen2 = dsl.parseString("a.test.$x(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(ctx_r[sen1c[-1]].type, sen2[-1].type)

    def test_apply_types_var_left(self):
        sen1 = dsl.parseString("a.test.$x")[0]
        sen2 = dsl.parseString("a.test.sentence(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-1].type)

    def test_apply_types_var_left_repeated(self):
        sen1 = dsl.parseString("a.test.$x.$x")[0]
        sen2 = dsl.parseString("a.test.sentence(::blah)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertTrue(ctx_r)
        self.assertEqual(sen1c[-1].type, sen2[-2].type)

    def test_apply_types_var_left_repeated_conflict(self):
        sen1 = dsl.parseString("a.test.$x.$x(::bloo)")[0]
        sen2 = dsl.parseString("a.test.sentence(::blah).awf(::$y)")[0]

        with self.assertRaises(TE.TypeConflictException):
            ctx_r = tuf.type_unify(sen1, sen2, CtxIns())



    def test_apply_type_var(self):
        sen1 = dsl.parseString("a.test.$x(::$y)")[0]
        sen2 = dsl.parseString("a.test.sentence(::blah.bloo)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)

        self.assertEqual(sen1c[-1].type, dsl.parseString("blah.bloo")[0])



    def test_apply_types_generalise(self):
        sen1 = dsl.parseString("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = dsl.parseString("a.test(::blah).sentence(::a.b)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())

        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-2].type, "_:blah")
        self.assertEqual(sen1c[-1].type, "_:a.b")

    def test_apply_types_with_vars(self):
        sen1 = dsl.parseString("a.test.sentence")[0]
        sen2 = dsl.parseString("a.test.$x(::blah!$y)")[0]

        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-1].type, "_:blah.y")
        self.assertTrue(sen1c[-1].type[-1].is_var)
        self.assertIn("y", ctx_r)
        self.assertIn(id(sen1[-1].type), ctx_r)

    def test_apply_types_with_vars_completed(self):
        sen1 = dsl.parseString("a.test.sentence.bloo(::aweg.awg)")[0]
        sen2 = dsl.parseString("a.test.$x(::blah!$y).$z(::$y)")[0]


        ctx_r = tuf.type_unify(sen1, sen2, CtxIns())
        sen1c = tuf.typed_sen_logic.apply(sen1, ctx_r)
        sen2c = tuf.typed_sen_logic.apply(sen2, ctx_r)

        self.assertEqual(sen1c[-1].type, "_:aweg.awg")
        self.assertEqual(sen1c[-2].type, "_:blah._:aweg.awg")
        self.assertEqual(sen1c[-2].type[-1], "_:aweg.awg")
        self.assertEqual(sen2c[-1].type, "_:aweg.awg")
        self.assertEqual(sen2c[-2].type, "_:blah._:aweg.awg")
        self.assertEqual(sen2c[-2].type[-1], "_:aweg.awg")
        self.assertNotEqual(sen1c, sen2c)
        self.assertNotEqual(sen1c[-2].type, "_:blah.y")


