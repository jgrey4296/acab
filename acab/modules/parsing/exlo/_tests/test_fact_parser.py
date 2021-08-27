import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

import random
import pyparsing as pp

import acab
config = acab.setup()

from acab.abstract.core import default_structure as DS
from acab.abstract.core.values import Sentence
from acab.abstract.core.values import AcabValue

from acab.abstract.parsing.parsers import HOTLOAD_VALUES, VALBIND

import acab.modules.parsing.exlo.parsers.FactParser as FP

NEGATION_S      = config.prepare("Parse.Structure", "NEGATION")()
TYPE_INSTANCE_S = config.prepare("Parse.Structure", "TYPE_INSTANCE")()

class Trie_Fact_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.WARN)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    #----------
    def test_trivial(self):
        self.assertIsNotNone(FP.parseString)
        self.assertIsNotNone(FP.PARAM_SEN)
        self.assertIsNotNone(FP.PARAM_SEN_PLURAL)

    def test_parseStrings(self):
        result = FP.parseString('a.b.c, b.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence) for x in result]))

    def test_parse_strings_multi_with_comma(self):
        result = FP.parseString('a.b.c, b.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence) for x in result]))

    def test_parse_strings_multi_line(self):
        result = FP.parseString('a.b.c\nb.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence) for x in result]))

    def test_param_fact_string(self):
        result = FP.PARAM_SEN.parseString('a.b.$x')[0]
        self.assertIsNotNone(result)
        self.assertTrue(result[-1].is_var)

    def test_exclusion_operator_parsing(self):
        result = FP.parseString('a!b!c')[0]
        self.assertTrue(all([x.data['exop'] == config.enums['exop'].EX for x in result[:-1]]))

    def test_strings(self):
        result = FP.parseString('a.b."This is a test"!c')[0]
        self.assertEqual(len(result), 4)
        self.assertEqual(result[2].value, 'This is a test')

    def test_bind_addition_to_node_recognition(self):
        result = FP.parseString('$a.$b!$c')[0]
        for x in result:
            self.assertTrue(x.is_var)

    def test_fact_leading_bind(self):
        result = FP.parseString('$x.a.b.c')[0]
        self.assertTrue(result[0].is_var)

    def test_valbind_expansion(self):
        """ Test added new parsers to the valbind parser """
        new_parser = pp.Word("¿awef")
        new_parser.setParseAction(lambda s, l, t: ("awef", t[0]))

        HOTLOAD_VALUES << new_parser

        a = VALBIND.parseString("¿awef")[0]
        self.assertEqual(a.value, "¿awef")
        self.assertEqual(a.type, Sentence.build(["awef"]))

    def test_negated_basic_sentence(self):
        result = FP.BASIC_SEN.parseString('~a.test!string')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(result.data[NEGATION_S])

    def test_positive_basic_sentence(self):
        result = FP.BASIC_SEN.parseString('a.test!string')[0]
        self.assertIsInstance(result, Sentence)
        self.assertFalse(result.data[NEGATION_S])

    def test_sentence_statement(self):
        result = FP.SEN_STATEMENT.parseString("a.test.sentence:\nextension.sentence\nsecond.extension\nend")
        sen1 = FP.BASIC_SEN.parseString('a.test.sentence.extension.sentence')[0]
        sen2 = FP.BASIC_SEN.parseString('a.test.sentence.second.extension')[0]

        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], sen1)
        self.assertEqual(result[1], sen2)

    def test_constraint(self):
        result = FP.PARAM_BINDING_END.parseString("test(λa.b.c $x)")[0]
        self.assertIsInstance(result, AcabValue)
        self.assertTrue(DS.CONSTRAINT in result.data)
        self.assertEqual(len(result.data[DS.CONSTRAINT][0].params), 1)

    def test_constraint_no_var(self):
        result = FP.PARAM_BINDING_END.parseString("test(λa.b.c)")[0]
        self.assertIsInstance(result, AcabValue)
        self.assertTrue(DS.CONSTRAINT in result.data)
        self.assertEqual(len(result.data[DS.CONSTRAINT][0].params), 0)

    def test_constraint_multi_var(self):
        result = FP.PARAM_BINDING_END.parseString("test(λa.b.c $x $y $z)")[0]
        self.assertIsInstance(result, AcabValue)
        self.assertTrue(DS.CONSTRAINT in result.data)
        self.assertEqual(len(result.data[DS.CONSTRAINT][0].params), 3)
        self.assertTrue(all([isinstance(x, AcabValue) for x in result.data[DS.CONSTRAINT][0].params]))
