import logging as logmod
import unittest
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import random
import warnings

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

import acab.modules.parsing.exlo.parsers.FactParser as FP
from acab.core.parsing import parsers as PU
from acab.core.parsing.annotation import ValueAnnotation
import acab.core.defaults.value_keys as DS
from acab.core.value.sentence import Sentence
from acab.interfaces.value import Sentence_i, Value_i


class Trie_Fact_Parser_Tests(unittest.TestCase):

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

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    #----------
    def test_trivial(self):
        """ Check basic elements of the parser exist """
        self.assertIsNotNone(FP.parse_string)
        self.assertIsNotNone(FP.SENTENCE)
        self.assertIsNotNone(FP.SEN_PLURAL)

    def test_parse_strings(self):
        """ Check multiple sentences can be parsed on separate lines"""
        result = FP.parse_string('a.b.c\nb.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence_i) for x in result]))

    def test_parse_strings_multi_with_comma(self):
        """ Check multiple strings can be parsed on one line """
        result = FP.parse_string('a.b.c, b.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence_i) for x in result]))

    def test_parse_strings_multi_line(self):
        result = FP.SEN_PLURAL.parse_string('a.b.c\nb.c.d')[:]
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence_i) for x in result]))

    def test_param_fact_string(self):
        result = FP.SENTENCE.parse_string('a.b.$x')[0]
        self.assertIsNotNone(result)
        self.assertTrue(result[-1].is_var)

    def test_exclusion_operator_parsing(self):
        """ Check exclusion operators can be parsed """
        result = FP.parse_string('a!b!c')[0]
        self.assertTrue(all([x.data['exop'] == config.enums['exop'].EX for x in result[:-1]]))

    def test_strings(self):
        """ Check strings can be parsed """
        result = FP.parse_string('a.b."This is a test"!c')[0]
        self.assertEqual(len(result), 4)
        self.assertEqual(result[2].value, 'This is a test')

    def test_bind_addition_to_node_recognition(self):
        """ Check variables can be parsed with exclusion operators """
        result = FP.parse_string('$a.$b!$c')[0]
        for x in result:
            self.assertTrue(x.is_var)

    def test_fact_leading_bind(self):
        """ Check a sentence with a variable at the head can be parsed """
        result = FP.parse_string('$x.a.b.c')[0]
        self.assertTrue(result[0].is_var)

    def test_valbind_expansion(self):
        """ Test added new parsers to the valbind parser """
        new_parser = pp.Word("¿awef")
        new_parser.set_parse_action(lambda s, l, t: ("awef", t[0]))

        PU.HOTLOAD_VALUES << new_parser

        a = PU.VALBIND.parse_string("¿awef")[0]
        self.assertEqual(a.value, "¿awef")
        self.assertEqual(a.type, Sentence(["awef"]))

    def test_negated_sentence(self):
        result = FP.SENTENCE.parse_string('~a.test!string')[0]
        self.assertIsInstance(result, Sentence_i)
        self.assertTrue(result.data[DS.NEGATION])

    def test_positive_sentence(self):
        result = FP.SENTENCE.parse_string('a.test!string')[0]
        self.assertIsInstance(result, Sentence_i)
        self.assertFalse(result.data[DS.NEGATION])


    def test_valbind_flatten(self):
        FP.HOTLOAD_ANNOTATIONS << FP.flatten_annotation
        result = FP.SEN_WORD.parse_string('$x(♭).')[0]
        self.assertIsInstance(result, Value_i)
        self.assertTrue(result.data[DS.FLATTEN])

    def test_valbind_no_flatten(self):
        FP.HOTLOAD_ANNOTATIONS << FP.flatten_annotation
        result = FP.SEN_WORD.parse_string('$x(~♭).')[0]
        self.assertIsInstance(result, Value_i)
        self.assertFalse(result.data[DS.FLATTEN])

    def test_valbind_no_flatten_as_sharp(self):
        FP.HOTLOAD_ANNOTATIONS << FP.flatten_annotation
        result = FP.SEN_WORD.parse_string('$x(♯).')[0]
        self.assertIsInstance(result, Value_i)
        self.assertFalse(result.data[DS.FLATTEN])

    def test_valbind_flatten_as_not_sharp(self):
        FP.HOTLOAD_ANNOTATIONS << FP.flatten_annotation
        result = FP.SEN_WORD.parse_string('$x(~♯).')[0]
        self.assertIsInstance(result, Value_i)
        self.assertTrue(result.data[DS.FLATTEN])

    def test_constraint(self):
        annotation = pp.Literal("blah")
        annotation.set_parse_action(lambda x: ValueAnnotation("blah", 4))
        FP.HOTLOAD_ANNOTATIONS << annotation
        result = FP.SEN_WORD.parse_string("test(blah)!")[0]
        self.assertIsInstance(result, Value_i)
        self.assertTrue("blah" in result.data)
        self.assertEqual(result.data["blah"], 4)
        FP.HOTLOAD_ANNOTATIONS << pp.Empty()


    def test_parse_op_sen(self):
        result = FP.SENTENCE.parse_string("λa.b.c")[0]
        self.assertIsInstance(result, Sentence_i)
        self.assertIn(DS.OPERATOR, result.type)

    @unittest.expectedFailure
    def test_nested_sentence(self):
        result = FP.SENTENCE.parse_string("a.test.[[nested.sentence]]")[0]
        self.assertIsInstance(result, Sentence_i)


    @unittest.skip("sentence macro is broken")
    def test_sentence_statement(self):

        result = FP.SENTENCE.parse_string("a.test.sentence:\n  extension.sentence\n  second.extension\nend")

        sen1 = FP.SENTENCE.parse_string('a.test.sentence.extension.sentence')[0]
        sen2 = FP.SENTENCE.parse_string('a.test.sentence.second.extension')[0]


        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], sen1)
        self.assertEqual(result[1], sen2)

    @unittest.skip("obsolete")
    def test_annotation_no_var(self):
        annotation = pp.Literal("blah")
        annotation.set_parse_action(lambda x: ValueAnnotation("blah", 4))
        FP.HOTLOAD_ANNOTATIONS << annotation
        result = FP.SEN_NO_MODAL.parse_string("test(blah)")[0]
        self.assertIsInstance(result, Value_i)
        self.assertTrue("blah" in result.data)
        self.assertEqual(result.data["blah"], 4)

    @unittest.skip("obsolete")
    def test_constraint_multi_var(self):
        result = FP.SEN_END.parse_string("test(λa.b.c $x $y $z)")[0]
        self.assertIsInstance(result, Value_i)
        self.assertTrue(DS.CONSTRAINT in result.data)
        self.assertEqual(len(result.data[DS.CONSTRAINT][0].params), 3)
        self.assertTrue(all([isinstance(x, Value_i) for x in result.data[DS.CONSTRAINT][0].params]))
