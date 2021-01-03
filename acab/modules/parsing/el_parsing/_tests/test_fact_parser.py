import unittest
from os.path import splitext, split
import logging as root_logger
logging = root_logger.getLogger(__name__)

import random
import pyparsing as pp

from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get("acab/abstract/config")

from acab.abstract.core.values import Sentence
from acab.abstract.core.values import AcabValue

from acab.abstract.parsing.parsers import HOTLOAD_VALUES, VALBIND

from acab.abstract.semantics.print_semantics import AcabPrintSemantics
from acab.abstract.printing import default_handlers as DH
from acab.working_memory.trie_wm import util as KBU
import acab.working_memory.trie_wm.parsing.FactParser as FP


NEGATION_S      = config.value("Parse.Structure", "NEGATION")
TYPE_INSTANCE_S = config.value("Parse.Structure", "TYPE_INSTANCE")

basic_plus = {AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel),
              Sentence: DH.DEF_SEN_PAIR}

Printer = AcabPrintSemantics(basic_plus, default_values={'MODAL_FIELD' : 'exop',
                                                         'EXOP.DOT'    : ".",
                                                         'EXOP.EX'     : "!"})

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

        self.assertEqual(Printer.print(result), "a.b.c")
        self.assertTrue(all([x.data['exop'] == config.modal_enums['exop'].DOT for x in result]))

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
        self.assertTrue(all([x.data['exop'] == config.modal_enums['exop'].EX for x in result[:-1]]))

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

    def test_fact_str_equal(self):
        # TODO needs str type wrapping
        actions = ["a.b.c",
                   "a.b!c",
                   'a.b."a string".c',
                   'a.b!"a string"!c',
                   'a.b.$x',
                   'a!$x!y']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)
        for act,par in zipped:
            self.assertEqual(act,Printer.print(par))

    def test_leading_bind_str_equal(self):
        # TODO needs var type wrapping
        actions = ['$x.a.b.c', '$y!b.c', '$x.$y!$z']
        parsed = [FP.parseString(x)[0] for x in actions]
        zipped = zip(actions, parsed)

        for a,p in zipped:
            self.assertEqual(a, Printer.print(p))

    def test_binding_expansion(self):
        bindings = { "a" : FP.parseString("blah")[0],
                     "b": FP.parseString("bloo")[0] }
        result = FP.parseString('$a.b.$b!c')[0]
        expanded = result.bind(bindings)
        asString = Printer.print(expanded)
        self.assertEqual(asString, "blah.b.bloo!c")

    def test_valbind_expansion(self):
        """ Test added new parsers to the valbind parser """
        new_parser = pp.Word("¿awef")
        new_parser.setParseAction(lambda t: ("awef", t[0]))

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
        result = FP.SEN_STATEMENT.parseString("a.test.sentence: (::Σ)\nextension.sentence\nsecond.extension\n end")
        sen1 = FP.BASIC_SEN.parseString('a.test.sentence.extension.sentence')[0]
        sen2 = FP.BASIC_SEN.parseString('a.test.sentence.second.extension')[0]

        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], sen1)
        self.assertEqual(result[1], sen2)

    def test_nested_sentence_statement(self):
        result = FP.SEN_STATEMENT.parseString("a.test.sentence: (::Σ)\ninternal.nested: (::Σ)\ninternal.one\ninternal.two\nend\nblah.bloo.blee\nend")
        self.assertEqual(len(result), 3)
        self.assertEqual(Printer.print(result[0]), "a.test.sentence.internal.nested.internal.one")
        self.assertEqual(Printer.print(result[1]), "a.test.sentence.internal.nested.internal.two")
        self.assertEqual(Printer.print(result[2]), "a.test.sentence.blah.bloo.blee")



