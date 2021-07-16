#!/usr/bin/env python3


import logging as root_logger
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = root_logger.getLogger(__name__)
##############################

import acab

config = acab.setup()

import acab.modules.parsing.exlo.parsers.FactParser as FP
import acab.modules.printing.printers as Printers
from acab.abstract.config.config import AcabConfig
from acab.abstract.core import default_structure as DS
from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator)
from acab.abstract.core.values import AcabStatement, AcabValue, Sentence
from acab.abstract.interfaces.printing_interfaces import PrintSystem

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
        sem = PrintSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                    Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL")],
                          structs=[])
        words = ["a", "b", "c", "d"]
        sentence = Sentence.build(words)

        result = sem.pprint(sentence)
        self.assertEqual(result, SEN_JOIN_S.join(words))

    def test_sentence_modal(self):
        sem = PrintSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test.sen")

    def test_sentence_modal_2(self):
        sem = PrintSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen")

    def test_sentence_query(self):
        sem = PrintSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        sentence.data[DS.QUERY] = True
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen?")

    def test_sentence_negated(self):
        sem = PrintSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("~a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "~a.test.sen")
