import logging as root_logger
import unittest
import unittest.mock as mock
# Setup root_logger:
from os.path import split, splitext

from acab import setup

config = setup()

import acab.modules.analysis.typing.util as util
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
        result = util.unify_sentences(sen1, sen2)
        self.assertIsInstance(result, CtxIns)
        self.assertFalse(result)


    def test_lax_basic(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence")[0]
        result = util.lax_unify(sen1, sen2)
        self.assertIsInstance(result, CtxIns)

    def test_lax_remainder(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence.blah")[0]
        result = util.lax_unify(sen1, sen2)

        self.assertIsInstance(result, CtxIns)


    def test_variable(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x")[0]

        result = util.unify_sentences(sen1, sen2)
        self.assertIsInstance(result, CtxIns)
        self.assertEqual(len(result), 1)
        self.assertIn('x', result)
        self.assertEqual(result['x'], "sentence")
        self.assertIsInstance(result['x'], AcabValue)

    def test_variable_with_gamma(self):
        gamma = CtxIns({ 'x' : dsl.parse_string("blah")[0]})
        sen1 = dsl.parse_string("a.test.$x")[0]
        sen2 = dsl.parse_string("a.test.$y")[0]

        result = util.unify_sentences(sen1, sen2, gamma=gamma)

        self.assertIsInstance(result, CtxIns)
        self.assertEqual(len(result), 2)
        self.assertIn('x', result)
        self.assertEqual(result['x'], "blah")
        self.assertIsInstance(result['x'], AcabValue)
        self.assertIn('y', result)
        self.assertEqual(result['y'], "blah")
    def test_unify_then_bind(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x")[0]

        subs = util.unify_sentences(sen1, sen2)

        sen3 = sen2.bind(subs)

        self.assertEqual(sen1, sen3)


    def test_unify_then_bind(self):
        total = dsl.parse_string("a.test.sentence")[0]
        sen1  = dsl.parse_string("a.$y.sentence")[0]
        sen2  = dsl.parse_string("a.test.$x")[0]

        subs  = util.unify_sentences(sen1, sen2)

        sen3  = sen2.bind(subs.data)

        self.assertEqual(total, sen3)


    def test_unify_fail_diff_lengths(self):
        sen1  = dsl.parse_string("a.$y.sentence")[0]
        sen2  = dsl.parse_string("a.test.sentence.d")[0]

        with self.assertRaises(TE.AcabTypingException):
            util.unify_sentences(sen1, sen2)

    def test_unify_fail(self):
        sen1  = dsl.parse_string("a.$x.sentence")[0]
        sen2  = dsl.parse_string("a.test.$x")[0]

        with self.assertRaises(TE.AcabTypingException):
            subs = util.unify_sentences(sen1, sen2)


    def test_unify_duplicate(self):
        sen1  = dsl.parse_string("a.$x.test")[0]
        sen2  = dsl.parse_string("a.test.$x")[0]

        subs = util.unify_sentences(sen1, sen2)

        self.assertTrue('x' in subs)

    def test_unify_vars(self):
        sen1  = dsl.parse_string("a.test.$x")[0]
        sen2  = dsl.parse_string("a.test.$y")[0]

        subs = util.unify_sentences(sen1, sen2)
        sen3  = sen1.bind(subs.data)

        self.assertTrue('x' in subs)
        self.assertEqual(sen2, sen3)

    def test_unify_conflict(self):
        sen1  = dsl.parse_string("a.test.$x.bloo")[0]
        sen2  = dsl.parse_string("a.test.blah.$x")[0]

        with self.assertRaises(TE.TypeUnifyException):
            gamma = util.unify_sentences(sen1, sen2)

    def test_unify_chain(self):
        sen1  = dsl.parse_string("a.test.$x.$y")[0]
        sen2  = dsl.parse_string("a.test.$y.bloo")[0]

        gamma = util.unify_sentences(sen1, sen2)
        self.assertIn("x", gamma)
        self.assertIn("y", gamma)
        self.assertEqual(gamma.x, "y")
        self.assertEqual(gamma.y, "bloo")


    def test_subtype_relation_eq(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.c")[0]

        self.assertIsInstance(util.lax_unify(sen1, sen2), CtxIns)

    def test_subtype_relation_fail(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("d.b.c")[0]

        with self.assertRaises(TE.TypeUnifyException):
            self.assertIsInstance(util.lax_unify(sen1, sen2), CtxIns)


    def test_subtype_relation_true_sub(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b")[0]

        self.assertIsInstance(util.lax_unify(sen1, sen2), CtxIns)


    def test_subtype_relation_right_var(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.$x")[0]
        result = util.lax_unify(sen1, sen2)
        self.assertIsInstance(result, CtxIns)
        self.assertEqual(result.x, "c")


    def test_subtype_relation_left_var(self):
        sen1 = dsl.parse_string("a.b.$x")[0]
        sen2 = dsl.parse_string("a.b")[0]
        result = util.lax_unify(sen1, sen2)
        self.assertIsInstance(result, CtxIns)
        self.assertNotIn("x", result)

    def test_subtype_relation_left_var_crit_path(self):
        sen1 = dsl.parse_string("a.$x.c")[0]
        sen2 = dsl.parse_string("a.b")[0]
        result = util.lax_unify(sen1, sen2)
        self.assertIsInstance(result, CtxIns)
        self.assertEqual(result.x, "b")


    def test_apply_types(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        result, gamma = util.unify_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        newly_typed = result[-1].type

        self.assertIn(newly_typed, gamma)
        self.assertEqual(gamma[newly_typed], sen2[-1].type)


    def test_apply_types_var(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x(::blah)")[0]

        result, gamma = util.unify_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        newly_typed = result[-1].type
        self.assertIn(newly_typed, gamma)
        self.assertEqual(gamma[newly_typed], sen2[-1].type)

    def test_apply_types_var_left(self):
        sen1 = dsl.parse_string("a.test.$x")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        result, gamma = util.unify_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        newly_typed = result[-1].type
        self.assertIn(newly_typed, gamma)
        self.assertEqual(gamma[newly_typed], sen2[-1].type)

    def test_apply_types_var_left_repeated(self):
        sen1 = dsl.parse_string("a.test.$x.$x")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        result, gamma = util.unify_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        newly_typed = result[-2].type

        self.assertIn(newly_typed, gamma)
        self.assertEqual(gamma[newly_typed], sen2[-1].type)

    def test_apply_types_var_left_repeated_conflict(self):
        sen1 = dsl.parse_string("a.test.$x.$x(::bloo)")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        with self.assertRaises(TE.TypeConflictException):
            result, gamma = util.unify_types(sen1, sen2)



    def test_apply_type_var(self):
        sen1 = dsl.parse_string("a.test.$x(::$y)")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        result, gamma = util.unify_types(sen1, sen2)

        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        self.assertEqual(gamma[result[-1].type], gamma.y)



    def test_apply_types_generalise(self):
        sen1 = dsl.parse_string("a.test(::blah.bloo).sentence(::a.b.c)")[0]
        sen2 = dsl.parse_string("a.test(::blah).sentence(::a.b)")[0]

        result, gamma = util.unify_types(sen1, sen2)

        breakpoint()
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))
        self.assertEqual(result[-2].type, "_:blah")
        self.assertEqual(result[-1].type, "_:a.b")


    # def test_apply_types_with_vars(self):
    #     sen1 = dsl.parse_string("a.test.sentence")[0]
    #     sen2 = dsl.parse_string("a.test.$x(::blah!$y)")[0]

    #     result, gamma = util.unify_types(sen1, sen2)

    #     self.assertIsInstance(result, Sentence)
    #     self.assertEqual(len(sen1), len(result))

    #     self.assertEqual(gamma[result[-1].type], "_:blah.y")
    #     self.assertTrue(gamma[result[-1].type][-1].is_var)




    # def test_typing(self):
    #     """
    #     Typing:
    #     Generate constraints
    #     solve constraints with unification
    #     """
    #     # gamma = CtxIns({ 'x' : dsl.parse_string("blah")[0]})
    #     gamma = CtxIns()
    #     sen1 = dsl.parse_string("a.test(::$y).$x(::$y)")[0]
    #     sen2 = dsl.parse_string("a.test(::bloo).sentence(::blah.bloo).other(::awef)")[0]

    #     result, gamma_prime = util.unify_types(sen1, sen2, gamma=gamma)

    #     self.assertIsInstance(result, Sentence)
