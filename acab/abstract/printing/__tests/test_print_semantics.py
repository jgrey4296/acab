#!/usr/bin/env python

from os.path import splitext, split
import unittest
import unittest.mock as mock
import re

import logging as root_logger
logging = root_logger.getLogger(__name__)
##############################

import acab
config = acab.setup()

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue, AcabStatement, Sentence
from acab.abstract.core.values import Sentence
from acab.abstract.core.production_abstractions import ProductionContainer, ProductionComponent, ProductionOperator

import acab.modules.semantics.printers as Printers
from acab.abstract.printing.print_types import RET_enum
from acab.abstract.printing import default_handlers as DH

NEGATION_S        = config.value("Value.Structure", "NEGATION")
QUERY_S           = config.value("Value.Structure", "QUERY")
BIND_S            = config.value("Value.Structure", "BIND")

NEGATION_SYMBOL_S = config.value("Symbols", "NEGATION")
ANON_VALUE_S      = config.value("Symbols", "ANON_VALUE")
FALLBACK_MODAL_S  = config.value("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])
QUERY_SYMBOL_S    = config.value("Symbols", "QUERY")

SEN_JOIN_S       = config.value("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])

STR_PRIM_S       = Sentence.build([config.value("Type.Primitive", "STRING")])
REGEX_PRIM_S     = Sentence.build([config.value("Type.Primitive", "REGEX")])
TYPE_INSTANCE_S  = config.value("Value.Structure", "TYPE_INSTANCE")


class PrintSemanticTests(unittest.TestCase):
    """ Test the basic Print Semantics using default settings """

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    def test_initial(self):
        sem = BasicPrinter()
        result = sem.print(AcabValue("Test"))
        self.assertEqual(result, "Test")

    def test_multiple(self):
        sem = BasicPrinter()
        result = sem.print(AcabValue("a"), AcabValue("b"), AcabValue("c"))
        self.assertEqual(result, r"a\nb\nc")


    def test_string_wrap(self):
        sem = PrimitiveTypeAwarePrinter()
        test = AcabValue(name="blah", data={TYPE_INSTANCE_S: STR_PRIM_S})
        result = sem.print(test)
        self.assertEqual(result, '"blah"')

    def test_regex_wrap(self):
        sem = PrimitiveTypeAwarePrinter()
        test = AcabValue(value=re.compile("blah"), data={TYPE_INSTANCE_S: REGEX_PRIM_S})
        result = sem.print(test)
        self.assertEqual(result, r'/blah/')

    def test_var_wrap(self):
        sem = PrimitiveTypeAwarePrinter()
        test = AcabValue(value=re.compile("blah"), data={BIND_S : True})
        result = sem.print(test)
        self.assertEqual(result, r'$blah')
    def test_modal_print(self):
        sem = PrimitiveTypeAwarePrinter()
        test = AcabValue(value=re.compile("blah"), data={BIND_S : True})
        result = sem.print(test)
        self.assertEqual(result, r'$blah')


    def test_symbol_override(self):
        sem = PrimitiveTypeAwarePrinter({("Symbols", "BIND"): "%"})
        test = AcabValue(value=re.compile("blah"), data={BIND_S : True})
        result = sem.print(test)
        self.assertEqual(result, r'%blah')



    def test_sentence_basic(self):
        sem = SentenceAwarePrinter(sub_map={'DEFAULT' : PrimitiveTypeAwarePrinter()})
        words = ["a", "b", "c", "d"]
        sentence = Sentence.build(words)
        result = sem.print(sentence)
        self.assertEqual(result, SEN_JOIN_S.join(words))

    @unittest.skip
    def test_sentence_words(self):
        join_str = "."
        sem = BasicPrinter(basic_plus,
                                 {SEN_JOIN_S : join_str})
        sentence = Sentence.build(["a", "b", "c", "d"])
        result = sem.print(sentence)
        self.assertEqual(result, join_str.join(["a", "b", "c", "d"]))

    @unittest.skip
    def test_sentence_words2(self):
        join_str = "-"
        sem = BasicPrinter(basic_plus,
                                 {SEN_JOIN_S : join_str})
        sentence = Sentence.build(["a", "b", "c", "d"])
        result = sem.print(sentence)
        self.assertEqual(result, join_str.join(["a", "b", "c", "d"]))

    @unittest.skip
    def test_sentence_negated(self):
        join_str = "."
        sem = BasicPrinter(basic_plus,
                                 {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["a","b","c","d"])
        sentence.data[NEGATION_S] = True

        result = sem.print(sentence)
        self.assertEqual(result, "{}{}".format(NEGATION_SYMBOL_S,
                                               join_str.join(["a", "b", "c", "d"])))

    # query, different join strs
    @unittest.skip
    def test_sentence_query(self):
        join_str = "."
        sem = BasicPrinter(basic_plus,
                                 {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["a","b","c","d"])
        sentence.data[QUERY_S] = True

        result = sem.print(sentence)
        self.assertEqual(result, "{}{}".format(join_str.join(["a", "b", "c", "d"]),
                                               QUERY_SYMBOL_S))

    @unittest.skip
    def test_sentence_words3(self):
        join_str = "."
        sem = BasicPrinter(basic_plus,
                                 {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["test","one","bumble","foo"])
        sentence.data[QUERY_S] = True

        result = sem.print(sentence)

        self.assertEqual(result, "{}{}".format(join_str.join(["test","one","bumble","foo"]),
                                               QUERY_SYMBOL_S))



    # value : show_uuid, not, variable, at var
    @unittest.skip
    def test_value_uuid(self):
        val = AcabValue("test")
        sem = BasicPrinter(basic_uuid)
        result = sem.print(val)
        self.assertEqual(result, "({} : {})".format(val.name, val.uuid))

    # Type Instance
    @unittest.skip
    def test_type_instance(self):
        instance = Sentence.build(["a","b","c"])
        sem = BasicPrinter(basic_plus)

        # TODO test type instance

    # drop end op, constraints
    @unittest.skip
    def test_value_drop_terminal_modality(self):
        sem = BasicPrinter({
            AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel)
        }, default_values={"BLAH" : " -~- "})
        value = AcabValue("Test", data={'MODALITY': "BLAH"})
        result = sem.print(value, overrides={'MODAL_FIELD': 'MODALITY'})
        self.assertEqual(result, "Test -~- ")

        sem.set_for_uuid(value.uuid, ["drop_modal"])
        result2 = sem.print(value)
        self.assertEqual(result2, "Test")

    @unittest.skip
    def test_component_simple(self):
        component = ProductionComponent(value=Sentence.build(["testop", "blah"]))
        sem = BasicPrinter({
            ProductionComponent: ([DH.component_substruct], DH.component_sentinel),
            Sentence: DH.DEF_SEN_PAIR,
            AcabValue: ([], lambda s, d, c, a, p: (RET_enum.SIMPLE, str(d), None, None))
        },
                                 {SEN_JOIN_S: "."})

        result = sem.print(component)
        self.assertEqual(result, "λtestop.blah")

    # test component params

    # test component rebind

    # test component sugar


    # AcabValue(Op)
    # TODO wrap specific types: int, str, regex

    # name, path, alias, tags,

    # Statement(Sentence, Component)
    # rebind, surgared, params, type print, path


    # Container(Query, Transform, Action)
    # clauses

    # Structured:(Rule, Agenda, Layer, Pipeline)




    @unittest.skip
    def test_pprint_at_var(self):
        # TODO update this
        value = AcabValue("test")
        value.data.update({BIND_S: AT_BIND_S})
        self.assertTrue(value.is_at_var)
        self.assertEqual(str(value), "test")



    # TODO ask, use, set_for_uuid, set_overrides
    # TODO Push and pop stack
    # TODO retrieve semantics
