import logging as root_logger
import unittest
import unittest.mock as mock
# Setup root_logger:
from os.path import split, splitext

from acab import setup

config = setup()

from acab.modules.analysis.typing import unify
from acab.core.data.acab_struct import AcabNode
from acab.core.data.default_structure import BIND
from acab.core.data.values import AcabValue, Sentence
from acab.core.parsing.trie_bootstrapper import TrieBootstrapper
from acab.modules.analysis.typing.typing_dsl import TypingDSL
from acab.modules.context.context_set import ContextInstance as CtxIns
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.analysis.typing import type_exceptions as TE

# Set up the parser to ease test setup
bootstrapper = TrieBootstrapper()
dsl = EXLO_Parser()
t_dsl = TypingDSL()
dsl.assert_parsers(bootstrapper)
t_dsl.assert_parsers(bootstrapper)
dsl.query_parsers(bootstrapper)
t_dsl.query_parsers(bootstrapper)

class SentenceUnifyTests(unittest.TestCase):

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
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence")[0]
        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertFalse(ctx_r)


    def test_lax_basic(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence")[0]
        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, strict=False, transform=unify.basic_unify_transform)
        self.assertIsInstance(ctx_r, CtxIns)

    def test_lax_remainder(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence.blah")[0]
        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, strict=False, transform=unify.basic_unify_transform)

        self.assertIsInstance(ctx_r, CtxIns)


    def test_variable(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 1)
        self.assertIn('x', ctx_r)
        self.assertEqual(ctx_r['x'], "sentence")
        self.assertIsInstance(ctx_r['x'], AcabValue)

    def test_variable_with_gamma(self):
        gamma = CtxIns({ 'x' : dsl.parse_string("blah")[0]})
        sen1 = dsl.parse_string("a.test.$x")[0]
        sen2 = dsl.parse_string("a.test.$y")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, gamma=gamma, transform=unify.basic_unify_transform)

        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(len(ctx_r), 2)
        self.assertIn('x', ctx_r)
        self.assertEqual(ctx_r['x'], "blah")
        self.assertIsInstance(ctx_r['x'], AcabValue)
        self.assertIn('y', ctx_r)
        self.assertEqual(ctx_r['y'], "blah")

    def test_unify_then_bind(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)

        sen3 = sen2.bind(ctx_r.data)

        self.assertEqual(sen1, sen3)


    def test_unify_then_bind2(self):
        total = dsl.parse_string("a.test.sentence")[0]
        sen1  = dsl.parse_string("a.$y.sentence")[0]
        sen2  = dsl.parse_string("a.test.$x")[0]

        sen_r, ctx_r= unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)

        sen3  = sen2.bind(ctx_r.data)

        self.assertEqual(total, sen3)


    def test_unify_fail_diff_lengths(self):
        sen1  = dsl.parse_string("a.$y.sentence")[0]
        sen2  = dsl.parse_string("a.test.sentence.d")[0]

        with self.assertRaises(TE.AcabTypingException):
            unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform, strict=True)

    def test_unify_fail(self):
        sen1  = dsl.parse_string("a.$x.sentence")[0]
        sen2  = dsl.parse_string("a.test.$x")[0]

        with self.assertRaises(TE.AcabTypingException):
            subs = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)


    def test_unify_duplicate(self):
        sen1  = dsl.parse_string("a.$x.test")[0]
        sen2  = dsl.parse_string("a.test.$x")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)

        self.assertTrue('x' in ctx_r)

    def test_unify_vars(self):
        sen1  = dsl.parse_string("a.test.$x")[0]
        sen2  = dsl.parse_string("a.test.$y")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        sen3  = sen1.bind(ctx_r.data)

        self.assertTrue('x' in ctx_r)
        self.assertEqual(sen2, sen3)

    def test_unify_conflict(self):
        sen1  = dsl.parse_string("a.test.$x.bloo")[0]
        sen2  = dsl.parse_string("a.test.blah.$x")[0]

        with self.assertRaises(TE.TypeUnifyException):
            gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)

    def test_unify_chain(self):
        sen1  = dsl.parse_string("a.test.$x.$y")[0]
        sen2  = dsl.parse_string("a.test.$y.bloo")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        self.assertIn("x", ctx_r)
        self.assertIn("y", ctx_r)
        self.assertEqual(ctx_r.x, "y")
        self.assertEqual(ctx_r.y, "bloo")


    def test_subtype_relation_eq(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.c")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        # TODO test more

    def test_subtype_relation_fail(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("d.b.c")[0]

        with self.assertRaises(TE.TypeUnifyException):
            unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)


    def test_subtype_relation_true_sub(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)


    def test_subtype_relation_right_var(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.$x")[0]
        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "c")


    def test_subtype_relation_left_var(self):
        sen1 = dsl.parse_string("a.b.$x")[0]
        sen2 = dsl.parse_string("a.b")[0]
        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertNotIn("x", ctx_r)

    def test_subtype_relation_left_var_crit_path(self):
        sen1 = dsl.parse_string("a.$x.c")[0]
        sen2 = dsl.parse_string("a.b")[0]
        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.basic_unify_transform)
        self.assertIsInstance(ctx_r, CtxIns)
        self.assertEqual(ctx_r.x, "b")


    def test_apply_types(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        sen_r, ctx_r = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)
        self.assertIsInstance(sen_r, Sentence)
        self.assertEqual(len(sen1), len(sen_r))

        newly_typed = sen_r[-1].type

        self.assertIn(newly_typed, ctx_r)
        self.assertEqual(ctx_r[newly_typed], sen2[-1].type)


    def test_apply_types_var(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x(::blah)")[0]

        ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)
        self.assertIsInstance(ctx_r, Sentence)
        self.assertEqual(len(sen1), len(ctx_r))

        newly_typed = ctx_r[-1].type
        self.assertIn(newly_typed, gamma)
        self.assertEqual(gamma[newly_typed], sen2[-1].type)

    def test_apply_types_var_left(self):
        sen1 = dsl.parse_string("a.test.$x")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]


        ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)
        self.assertIsInstance(ctx_r, Sentence)
        self.assertEqual(len(sen1), len(ctx_r))

        newly_typed = ctx_r[-1].type
        self.assertIn(newly_typed, gamma)
        self.assertEqual(gamma[newly_typed], sen2[-1].type)

    def test_apply_types_var_left_repeated(self):
        sen1 = dsl.parse_string("a.test.$x.$x")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)
        self.assertIsInstance(ctx_r, Sentence)
        self.assertEqual(len(sen1), len(ctx_r))

        newly_typed = ctx_r[-2].type

        self.assertIn(newly_typed, gamma)
        self.assertEqual(gamma[newly_typed], sen2[-1].type)

    def test_apply_types_var_left_repeated_conflict(self):
        sen1 = dsl.parse_string("a.test.$x.$x(::bloo)")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        with self.assertRaises(TE.TypeConflictException):
            ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                     remainder_op=unify.type_remainder_transform)


    def test_apply_type_var(self):
        sen1 = dsl.parse_string("a.test.$x(::$y)")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)

        self.assertIsInstance(ctx_r, Sentence)
        self.assertEqual(len(sen1), len(ctx_r))

        self.assertEqual(gamma[ctx_r[-1].type], gamma.y)



    @unittest.skip
    def test_apply_types_generalise(self):
        sen1 = dsl.parse_string("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = dsl.parse_string("a.test(::blah).sentence(::a.b)")[0]

        ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)

        breakpoint()
        self.assertIsInstance(ctx_r, Sentence)
        self.assertEqual(len(sen1), len(ctx_r))
        self.assertEqual(ctx_r[-2].type, "_:blah")
        self.assertEqual(ctx_r[-1].type, "_:a.b")

    @unittest.skip
    def test_apply_types_with_vars(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x(::blah!$y)")[0]

        ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)

        self.assertIsInstance(ctx_r, Sentence)
        self.assertEqual(len(sen1), len(ctx_r))

        self.assertEqual(gamma[ctx_r[-1].type], "_:blah.y")
        self.assertTrue(gamma[ctx_r[-1].type][-1].is_var)


    @unittest.skip
    def test_typing(self):
        """
        Typing:
        Generate constraints
        solve constraints with unification
        """
        # gamma = CtxIns({ 'x' : dsl.parse_string("blah")[0]})
        gamma = CtxIns()
        sen1 = dsl.parse_string("a.test(::$y).$x(::$y)")[0]
        sen2 = dsl.parse_string("a.test(::bloo).sentence(::blah.bloo).other(::awef)")[0]

        ctx_r, gamma = unify.unify_sentence_pair(sen1, sen2, transform=unify.type_unify_transform,
                                                 remainder_op=unify.type_remainder_transform)

        self.assertIsInstance(ctx_r, Sentence)
