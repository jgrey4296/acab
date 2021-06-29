#!/usr/bin/env python3


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

from acab.abstract.interfaces.semantic_interfaces import PrintSemanticSystem
import acab.modules.semantics.printers as Printers
from acab.abstract.core import default_structure as DS

import acab.modules.parsing.exlo.parsers.FactParser as FP

NEGATION_S        = config.prepare("Value.Structure", "NEGATION")()
QUERY_S           = config.prepare("Value.Structure", "QUERY")()
BIND_S            = config.prepare("Value.Structure", "BIND")()
AT_BIND_S         = config.prepare("Value.Structure", "AT_BIND")()

NEGATION_SYMBOL_S = config.prepare("Symbols", "NEGATION")()
ANON_VALUE_S      = config.prepare("Symbols", "ANON_VALUE")()
FALLBACK_MODAL_S  = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()
QUERY_SYMBOL_S    = config.prepare("Symbols", "QUERY")()

SEN_JOIN_S        = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])()

STR_PRIM_S        = Sentence.build([config.prepare("Type.Primitive", "STRING")()])
REGEX_PRIM_S      = Sentence.build([config.prepare("Type.Primitive", "REGEX")()])
TYPE_INSTANCE_S   = config.prepare("Value.Structure", "TYPE_INSTANCE")()

class PrintBasicSentenceSemanticTests(unittest.TestCase):
    def test_sentence_basic(self):
        sem = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                            Printers.BasicSentenceAwarePrinter("_:SENTENCE")])
        words = ["a", "b", "c", "d"]
        sentence = Sentence.build(words)

        result = sem.pprint(sentence)
        self.assertEqual(result, SEN_JOIN_S.join(words))

    def test_sentence_modal(self):
        sem = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                            Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                            Printers.ConfigBackedSymbolPrinter("_:SYMBOL")],
                                  settings={"MODAL" : "exop"})
        sentence = FP.parseString("a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test.sen")

    def test_sentence_modal_2(self):
        sem = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                            Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                            Printers.ConfigBackedSymbolPrinter("_:SYMBOL")],
                                            settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen")

    def test_sentence_query(self):
        sem = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                            Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                            Printers.ConfigBackedSymbolPrinter("_:SYMBOL")],
                                            settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        sentence.data[DS.QUERY] = True
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen?")

    def test_sentence_negated(self):
        sem = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                            Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                            Printers.ConfigBackedSymbolPrinter("_:SYMBOL")],
                                  settings={"MODAL" : "exop"})
        sentence = FP.parseString("~a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "~a.test.sen")
