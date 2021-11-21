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
from acab.core.parsing.trie_bootstrapper import TrieBootstrapper
from acab.modules.analysis.typing import type_exceptions as TE
from acab.modules.analysis.typing import unify
from acab.modules.analysis.typing.typing_dsl import TypingDSL
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.context.context_set import MutableContextInstance
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser

# Set up the parser to ease test setup
bootstrapper = TrieBootstrapper()
dsl = EXLO_Parser()
t_dsl = TypingDSL()
dsl.assert_parsers(bootstrapper)
t_dsl.assert_parsers(bootstrapper)
dsl.query_parsers(bootstrapper)
t_dsl.query_parsers(bootstrapper)

class UnifyUtilTests(unittest.TestCase):

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


    def test_gen_type_vars_nop(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.c")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertFalse(ctx_r)

    def test_gen_type_vars_basic(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.c(::blah.bloo)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())
        self.assertNotIn(sen1[-1].type, ctx_r)


    def test_gen_type_vars_length_mismatch(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.c.d")[0]

        with self.assertRaises(TE.AcabMiscTypingException):
            ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

    def test_gen_type_vars_length_mismatch_2(self):
        sen1 = dsl.parse_string("a.b.c.d")[0]
        sen2 = dsl.parse_string("a.b.c")[0]

        with self.assertRaises(TE.AcabMiscTypingException):
            ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())


    def test_gen_type_vars_with_value_var(self):
        sen1 = dsl.parse_string("a.test.$x")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah.bloo)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertIn(sen1[-1], ctx_r)

    def test_gen_type_vars_existing_type_var(self):
        sen1 = dsl.parse_string("a.test.sentence(::$y)")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah.bloo)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertIn(sen1[-1].type[0], ctx_r)

    def test_gen_type_var_l_will_change(self):
        sen1 = dsl.parse_string("a.test")[0]
        sen2 = dsl.parse_string("a.test(::blah.bloo)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertNotIn(sen1[-1].type, ctx_r)
        self.assertNotIn(sen1[-1], ctx_r)

    def test_gen_type_var_override(self):
        sen1 = dsl.parse_string("a.test(::$x)")[0]
        sen2 = dsl.parse_string("a.test(::blah.bloo)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertIn(sen1[-1].type[0], ctx_r)
        self.assertNotIn(sen1[-1], ctx_r)

    def test_gen_type_var_canon_and_override(self):
        sen1 = dsl.parse_string("a.$x")[0]
        sen2 = dsl.parse_string("a.test(::blah.bloo)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertNotIn(sen1[-1].type, ctx_r)
        self.assertIn(sen1[-1], ctx_r)

    def test_apply_substitutions(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen1s = suf.apply_substitutions(sen1, CtxIns())

        self.assertEqual(sen1, sen1s)

    def test_apply_substitutions_2(self):
        sen1 = dsl.parse_string("a.b.$x")[0]
        sen1s = suf.apply_substitutions(sen1, CtxIns({"x" : "blah"}))

        sen_target = dsl.parse_string("a.b.blah")[0]

        self.assertEqual(sen1s, sen_target)

    def test_apply_type_substitution_type_var(self):
        sen1 = dsl.parse_string("a.b.c(::$x)")[0]
        sen1s = tuf.apply_types_sub(sen1, CtxIns())

        sen_target = dsl.parse_string("a.b.c(::$x)")[0]

        self.assertEqual(sen1s, sen_target)

    def test_apply_type_substitution_type_var2(self):
        sen1 = dsl.parse_string("a.b.c(::$x)")[0]
        sen1s = tuf.apply_types_sub(sen1, CtxIns({'x': dsl.parse_string("blah.bloo")[0]}))

        sen_target = dsl.parse_string("a.b.c(::blah.bloo)")[0]

        self.assertEqual(sen1s, sen_target)

    def test_apply_type_substitution_type_var2(self):
        sen1 = dsl.parse_string("a.b.$c(::$x)")[0]
        sen1s = tuf.apply_types_sub(sen1, CtxIns({'x': dsl.parse_string("blah.bloo")[0]}))

        sen_target = dsl.parse_string("a.b.$c(::blah.bloo)")[0]

        self.assertEqual(sen1s, sen_target)

    def test_var_repeat_chain(self):
        """
        Check multiples of variables create a single new var
        """
        sen1 = dsl.parse_string("a.test.$x.$x.$x")[0]
        sen2 = dsl.parse_string("a.test.b(::blah).b.b")[0]

        ctx_p = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertEqual(len(ctx_p.data), 1)
        self.assertIn("x", ctx_p)
        self.assertIn(sen1[2], ctx_p)
        self.assertNotIn(sen1[2].type, ctx_p)
        self.assertNotIn(sen1[3].type, ctx_p)


    def test_gen_type_var_r_will_change(self):
        sen1 = dsl.parse_string("a.test(::blah.bloo)")[0]
        sen2 = dsl.parse_string("a.test(::$x)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertIn(sen2[-1].type, ctx_r)
        self.assertNotIn(sen2[-1], ctx_r)

    def test_gen_type_var_r_will_change(self):
        sen1 = dsl.parse_string("a.test(::blah.bloo).blah")[0]
        sen2 = dsl.parse_string("a.test(::$x).awef(::$x)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertIn(sen2[1].type[0], ctx_r)
        self.assertNotIn(sen2[-2], ctx_r)

    def test_gen_types_generalise(self):
        sen1 = dsl.parse_string("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = dsl.parse_string("a.test(::blah).sentence(::a.b)")[0]

        ctx_r = tuf.gen_type_vars(sen1, sen2, CtxIns())

        self.assertEqual(len(ctx_r), 0)