#!/usr/bin/env python

#https://docs.python.org/3/library/unittest.html
# https://docs.python.org/3/library/unittest.mock.html

from os.path import splitext, split

import unittest
import unittest.mock as mock

import logging

from acab.config import GET
util = GET("acab/util.config")

NEGATION_SYMBOL_S = util("Visual.Symbols", "NEGATION_SYMBOL_S")
NEGATION_S       = util("Parsing.Structure", "NEGATION_S")
ANON_VALUE_S = util("Printing", "ANON_VALUE_S")
SEN_JOIN_S = util("Printing", "SEN_JOIN_S")
QUERY_S = util("Parsing.Structure", "QUERY_S")
QUERY_SYMBOL_S = util("Visual.Symbols", "QUERY_SYMBOL_S")

MODAL_SYMBOLS = util("WorkingMemory.TrieWM.Symbols", as_dict=True)

from acab.abstract.core.value import AcabValue, AcabStatement
from acab.abstract.core.sentence import Sentence
from acab.abstract.rule.production_operator import ProductionContainer
from acab.abstract.rule.query import Query
from acab.abstract.rule.transform import Transform
from acab.abstract.rule.action import Action
from acab.abstract.rule.rule import Rule
from acab.abstract.pipeline.layer import Layer
from acab.abstract.pipeline.agenda import Agenda
from acab.abstract.pipeline.pipeline import Pipeline

from acab.abstract.printing.print_semantics import AcabPrintSemantics, RET_enum

from acab.abstract.printing import default_handlers as DH

from acab.abstract.core.type_system import build_simple_type_system

# Semantic Collections
# Dict[type, Tuple[List[Callable], Callable, Any]]
basic = {AcabValue: ([], lambda s, d, c, a, p: (RET_enum.SIMPLE, str(d), None))}

basic_plus = {AcabValue: DH.DEF_VALUE_PAIR,
              Sentence: DH.DEF_SEN_PAIR,
              AcabStatement: ([], None),
              ProductionContainer: DH.DEF_CONTAINER_PAIR,
              Query: ([], None),
              Transform: ([], None),
              Action: ([], None),
              Rule: ([], None),
              Agenda: ([], None),
              Layer: ([], None),
              Pipeline: ([], None)
              }

basic_uuid = {AcabValue: DH.DEF_UUID_PAIR}


class PrintSemanticTests(unittest.TestCase):
    """ Test the basic Print Semantics using default settings """

    @classmethod
    def setUpClass(cls):
        # setup class
        type_sys = build_simple_type_system()
    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    # use testcase snippet
    # mock.Mock / MagicMock
    # create_autospec
    # @patch(' ') / with patch.object(...)

    def test_initial(self):
        sem = AcabPrintSemantics(basic)
        result = sem.print(AcabValue("Test"))
        self.assertEqual(result, "Test")

    def test_multiple(self):
        sem = AcabPrintSemantics(basic)
        result = sem.print([AcabValue("a"), AcabValue("b"), AcabValue("c")])
        self.assertEqual(result, r"a\nb\nc")

    def test_multiple2(self):
        sem = AcabPrintSemantics(basic,
                                 default_values={'print_sen_join': " -- "})
        result = sem.print([AcabValue("a"), AcabValue("b"), AcabValue("c")])
        self.assertEqual(result, "a -- b -- c")


    def test_sentence_basic(self):
        sem = AcabPrintSemantics(basic)
        sentence = Sentence.build(["a", "b", "c", "d"])
        result = sem.print(sentence)
        self.assertEqual(result, ANON_VALUE_S)

    def test_sentence_words(self):
        join_str = "."
        sem = AcabPrintSemantics(basic_plus,
                                 {SEN_JOIN_S : join_str})
        sentence = Sentence.build(["a", "b", "c", "d"])
        result = sem.print(sentence)
        self.assertEqual(result, join_str.join(["a", "b", "c", "d"]))

    def test_sentence_words2(self):
        join_str = "-"
        sem = AcabPrintSemantics(basic_plus,
                                 {SEN_JOIN_S : join_str})
        sentence = Sentence.build(["a", "b", "c", "d"])
        result = sem.print(sentence)
        self.assertEqual(result, join_str.join(["a", "b", "c", "d"]))

    def test_sentence_negated(self):
        join_str = "."
        sem = AcabPrintSemantics(basic_plus,
                                 {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["a","b","c","d"])
        sentence._data[NEGATION_S] = True

        result = sem.print(sentence)
        self.assertEqual(result, "{}{}".format(NEGATION_SYMBOL_S,
                                               join_str.join(["a", "b", "c", "d"])))

    # query, different join strs
    def test_sentence_query(self):
        join_str = "."
        sem = AcabPrintSemantics(basic_plus,
                                 {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["a","b","c","d"])
        sentence._data[QUERY_S] = True

        result = sem.print(sentence)
        self.assertEqual(result, "{}{}".format(join_str.join(["a", "b", "c", "d"]),
                                               QUERY_SYMBOL_S))

    def test_sentence_words(self):
        join_str = "."
        sem = AcabPrintSemantics(basic_plus,
                                 {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["test","one","bumble","foo"])
        sentence._data[QUERY_S] = True

        result = sem.print(sentence)
        self.assertEqual(result, "{}{}".format(join_str.join(["test","one","bumble","foo"]),
                                               QUERY_SYMBOL_S))



    # value : show_uuid, not, variable, at var
    def test_value_uuid(self):
        val = AcabValue("test")
        sem = AcabPrintSemantics(basic_uuid)
        result = sem.print(val)
        self.assertEqual(result, "({} : {})".format(val.name, val._uuid))

    # Type Instance
    def test_type_instance(self):
        instance = Sentence.build(["a","b","c"])
        sem = AcabPrintSemantics(basic_plus)

    # drop end op, constraints
    def test_value_drop_terminal_modality(self):
        # TODO register a modality str pair
        sem = AcabPrintSemantics({
            AcabValue: ([DH.value_name_accumulator, DH.modality_accumulator], DH.value_sentinel)
        }, default_values={"BLAH" : " -~- "})
        value = AcabValue("Test", data={'MODALITY': "BLAH"})
        result = sem.print(value, overrides={'MODAL_FIELD': 'MODALITY'})
        self.assertEqual(result, "Test -~- ")

        sem.set_for_uuid(value._uuid, ["drop_modal"])
        result2 = sem.print(value)
        self.assertEqual(result2, "Test")



    # AcabValue(Op)
    # name, path, alias, tags,

    # Statement(Sentence, Component)
    # rebind, surgared, params, type print, path


    # Container(Query, Transform, Action)
    # clauses

    # Structured:(Rule, Agenda, Layer, Pipeline)



if __name__ == "__main__":
    #run python $filename to use this logging setup
    #using python -m unittest $filename won't
    LOGLEVEL = logging.INFO
    logFileName = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.WARN)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
