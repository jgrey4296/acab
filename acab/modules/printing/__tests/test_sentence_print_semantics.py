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
from acab.modules.printing.basic_printer import BasicPrinter
from acab.abstract.interfaces.handler_system import Handler

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
        sem = BasicPrinter(handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                    Printers.PrimitiveTypeAwarePrinter().as_handler("_:NO_MODAL")],
                          structs=[])
        words = ["a", "b", "c", "d"]
        sentence = Sentence.build(words)

        result = sem.pprint(sentence)
        self.assertEqual(result, SEN_JOIN_S.join(words))

    def test_sentence_modal(self):
        sem = BasicPrinter(handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter().as_handler("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test.sen")

    def test_sentence_modal_2(self):
        sem = BasicPrinter(handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter().as_handler("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen")

    def test_sentence_query(self):
        sem = BasicPrinter(handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter().as_handler("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        sentence.data[DS.QUERY] = True
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen?")

    def test_sentence_negated(self):
        sem = BasicPrinter(handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                    Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                    Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                    Printers.PrimitiveTypeAwarePrinter().as_handler("_:NO_MODAL")],
                          structs=[],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("~a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "~a.test.sen")
