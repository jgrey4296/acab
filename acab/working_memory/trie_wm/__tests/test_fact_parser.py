import unittest
import logging
import random
import pyparsing as pp

from acab.config import AcabConfig
AcabConfig.Get().read("acab/util.config")

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue
from acab.working_memory.trie_wm import util as KBU
import acab.working_memory.trie_wm.parsing.FactParser as FP

NEGATION_S = AcabConfig.Get()("Parsing.Structure", "NEGATION_S")
VALUE_TYPE_S = AcabConfig.Get()("Parsing.Structure", "VALUE_TYPE_S")

class Trie_Fact_Parser_Tests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        return

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    def test_trivial(self):
        self.assertIsNotNone(FP.parseString)
        self.assertIsNotNone(FP.PARAM_SEN)
        self.assertIsNotNone(FP.PARAM_SEN_PLURAL)

    def test_parseString(self):
        result = FP.parseString('a.b.c')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(all([isinstance(x, AcabValue) for x in result]))
        self.assertEqual(result.pprint(), "a.b.c")
        self.assertTrue(all([x._data[KBU.OPERATOR_S] == KBU.EXOP.DOT for x in result]))

    def test_parseStrings(self):
        result = FP.parseString('a.b.c, b.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence) for x in result]))

    def test_parse_strings_multiline(self):
        result = FP.parseString('a.b.c\n b.c.d')
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        self.assertTrue(all([isinstance(x, Sentence) for x in result]))

    def test_param_fact_string(self):
        result = FP.PARAM_SEN.parseString('a.b.$x')[0]
        self.assertIsNotNone(result)
        self.assertTrue(result[-1].is_var)

    def test_exclusion_operator_parsing(self):
        result = FP.parseString('a!b!c')[0]
        self.assertTrue(all([x._data[KBU.OPERATOR_S] == KBU.EXOP.EX for x in result[:-1]]))

    def test_strings(self):
        result = FP.parseString('a.b."This is a test"!c')[0]
        self.assertEqual(len(result), 4)
        self.assertEqual(result[2]._value, "This is a test")

    def test_bind_addition_to_node_recognition(self):
        result = FP.parseString('$a.$b!$c')[0]
        for x in result:
            self.assertTrue(x.is_var)

    def test_fact_leading_bind(self):
        result = FP.parseString('$x.a.b.c')[0]
        self.assertTrue(result[0].is_var)

    def test_fact_str_equal(self):
        actions = ["a.b.c",
                   "a.b!c",
                   'a.b."a string".c',
                   'a.b!"a string"!c',
                   'a.b.$x',
                   'a!$x!y']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for a,p in zipped:
            self.assertEqual(a,p.pprint())

    def test_leading_bind_str_equal(self):
        actions = ['$x.a.b.c', '$y!b.c', '$x.$y!$z']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for a,p in zipped:
            self.assertEqual(a, p.pprint())

    def test_binding_expansion(self):
        bindings = { "a" : FP.parseString("blah")[0],
                     "b": FP.parseString("bloo")[0] }
        result = FP.parseString('$a.b.$b!c')[0]
        expanded = result.bind(bindings)
        asString = expanded.pprint()
        self.assertEqual(asString, "blah.b.bloo!c")

    def test_valbind_expansion(self):
        """ Test added new parsers to the valbind parser """
        new_parser = pp.Word("¿awef")
        new_parser.setParseAction(lambda t: ("awef", t[0]))

        FP.HOTLOAD_VALUES << new_parser

        a = FP.VALBIND.parseString("¿awef")[0]
        self.assertEqual(a._value, "¿awef")
        self.assertEqual(a._data[VALUE_TYPE_S], "awef")

    def test_negated_basic_sentence(self):
        result = FP.BASIC_SEN.parseString('~a.test!string')[0]
        self.assertIsInstance(result, Sentence)
        self.assertTrue(result._data[NEGATION_S])

    def test_positive_basic_sentence(self):
        result = FP.BASIC_SEN.parseString('a.test!string')[0]
        self.assertIsInstance(result, Sentence)
        self.assertFalse(result._data[NEGATION_S])

    def test_sentence_statement(self):
        result = FP.SEN_STATEMENT.parseString("a.test.sentence: (::Σ)\nextension.sentence\nsecond.extension\n end")
        sen1 = FP.BASIC_SEN.parseString('a.test.sentence.extension.sentence')[0]
        sen2 = FP.BASIC_SEN.parseString('a.test.sentence.second.extension')[0]

        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], sen1)
        self.assertEqual(result[1], sen2)

    def test_nested_sentence_statement(self):
        result = FP.SEN_STATEMENT.parseString("a.test.sentence: (::Σ)\ninternal.nested: (::Σ)\ninternal.one\ninternal.two\nend\nblah.bloo.blee\nend")
        self.assertEqual(len(result), 3)
        self.assertEqual(result[0].pprint(), "a.test.sentence.internal.nested.internal.one")
        self.assertEqual(result[1].pprint(), "a.test.sentence.internal.nested.internal.two")
        self.assertEqual(result[2].pprint(), "a.test.sentence.blah.bloo.blee")



if __name__ == "__main__":
    LOGLEVEL = logging.INFO
    logFileName = "log.trie_fact_parser_tests"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
