#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

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
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.error.semantic_exception import AcabSemanticException

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
        self.assertIsInstance(result, dict)
        self.assertFalse(result)


    def test_lax_basic(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence")[0]
        result = util.lax_unify_sentences(sen1, sen2)
        self.assertIsInstance(result, tuple)
        self.assertIsInstance(result[0], dict)
        self.assertIsInstance(result[1], Sentence)
        self.assertIsInstance(result[2], Sentence)
        self.assertEqual(0, len(result[1]))
        self.assertEqual(0, len(result[2]))

    def test_lax_remainder(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence.blah")[0]
        result = util.lax_unify_sentences(sen1, sen2)
        self.assertIsInstance(result, tuple)
        self.assertIsInstance(result[0], dict)
        self.assertIsInstance(result[1], Sentence)
        self.assertIsInstance(result[2], Sentence)
        self.assertEqual(0, len(result[1]))
        self.assertEqual(1, len(result[2]))


    def test_variable(self):
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x")[0]

        result = util.unify_sentences(sen1, sen2)
        self.assertIsInstance(result, dict)
        self.assertEqual(len(result), 1)
        self.assertIn('x', result)
        self.assertEqual(result['x'], "sentence")
        self.assertIsInstance(result['x'], AcabValue)

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

        sen3  = sen2.bind(subs)

        self.assertEqual(total, sen3)


    def test_unify_fail_diff_lengths(self):
        sen1  = dsl.parse_string("a.$y.sentence")[0]
        sen2  = dsl.parse_string("a.test.sentence.d")[0]

        with self.assertRaises(AcabSemanticException):
            util.unify_sentences(sen1, sen2)

    def test_unify_fail(self):
        sen1  = dsl.parse_string("a.$x.sentence")[0]
        sen2  = dsl.parse_string("a.test.$x")[0]

        with self.assertRaises(AcabSemanticException):
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
        sen3  = sen1.bind(subs)

        self.assertTrue('x' in subs)
        self.assertEqual(sen2, sen3)




    def test_subtype_relation_eq(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.c")[0]

        self.assertTrue(util.subtype_relation(sen1, sen2))

    def test_subtype_relation_fail(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("d.b.c")[0]

        self.assertFalse(util.subtype_relation(sen1, sen2))

    def test_subtype_relation_true_sub(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b")[0]

        self.assertTrue(util.subtype_relation(sen1, sen2))


    def test_subtype_relation_right_var(self):
        sen1 = dsl.parse_string("a.b.c")[0]
        sen2 = dsl.parse_string("a.b.$x")[0]

        self.assertTrue(util.subtype_relation(sen1, sen2))


    def test_subtype_relation_left_var(self):
        sen1 = dsl.parse_string("a.b.$x")[0]
        sen2 = dsl.parse_string("a.b")[0]

        self.assertTrue(util.subtype_relation(sen1, sen2))

    def test_subtype_relation_left_var_crit_path(self):
        sen1 = dsl.parse_string("a.$x.c")[0]
        sen2 = dsl.parse_string("a.b")[0]

        self.assertTrue(util.subtype_relation(sen1, sen2))


    def test_apply_types(self):
        """
        a.b.c ∪ a.b(::blah).d -> a.b(::blah).c
        """
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        result = util.apply_sentence_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        self.assertEqual(result[-1].type, "_:blah")


    def test_apply_types_var(self):
        """
        a.b.c ∪ a.b(::blah).d -> a.b(::blah).c
        """
        sen1 = dsl.parse_string("a.test.sentence")[0]
        sen2 = dsl.parse_string("a.test.$x(::blah)")[0]

        result = util.apply_sentence_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        self.assertEqual(result[-1].type, "_:blah")

    def test_apply_types_var_left(self):
        """
        a.b.c ∪ a.b(::blah).d -> a.b(::blah).c
        """
        sen1 = dsl.parse_string("a.test.$x")[0]
        sen2 = dsl.parse_string("a.test.sentence(::blah)")[0]

        result = util.apply_sentence_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        self.assertEqual(result[-1].type, "_:blah")

    def test_apply_types_only_subtypes(self):
        """
        a.b.c ∪ a.b(::blah).d -> a.b(::blah).c
        """
        sen1 = dsl.parse_string("a.test(::bloo).sentence(::a.b.c)")[0]
        sen2 = dsl.parse_string("a.test(::blah).sentence(::a.b)")[0]

        result = util.apply_sentence_types(sen1, sen2)
        self.assertIsInstance(result, Sentence)
        self.assertEqual(len(sen1), len(result))

        self.assertEqual(result[-2].type, "_:bloo")
        self.assertEqual(result[-1].type, "_:a.b")


    def test_unify_different_length_sentences(self):
        """
        a.$x.c ∪ a.b.c.d -> { $x: b } + [None, [d]]
        """
        pass


    def test_node_to_parallel_record_fields(self):
        """
        type: c, d end

        a.b(::type).c
        a.b.d
        """
        pass

    def test_node_to_sequential_record_fields(self):
        """
        type: c.d.e end

        a.b(::type).c.d.e
        """
        pass

    def test_node_to_sum_type(self):
        """
        type(::sum): c d e end

        a.b.c(::type)
        """
        pass

    def test_get_sentence_types(self):
        pass

    def test_weak_unify(self):
        """
        ∃ s in A, s.t: s ∪ n
        s, n :: Sentence
        A    :: Type
        """
        pass

    def test_strong_unify(self):
        """
        ∀ s in A: s.t: s ∪ n
        s :: Sentence
        n :: Node
        A :: Type
        """
        pass

    def test_type_apply(self):
        pass


    def test_sentence_to_parallel_record_fields(self):
        """
        type: c, d end

        a.b(::type).c
        a.b.d
        """
        pass

    def test_sentence_to_sequential_record_fields(self):
        """
        type: c.d.e end

        a.b(::type).c.d.e
        """
        pass

    def test_sentence_to_sum_type(self):
        """
        type(::sum): c d e end

        a.b.c(::type)
        """
        pass
