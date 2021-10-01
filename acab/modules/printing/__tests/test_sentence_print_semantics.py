#!/usr/bin/env python3


import logging as root_logger
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext

import pyparsing as pp

logging = root_logger.getLogger(__name__)
##############################

import acab

config = acab.setup()


import acab.modules.parsing.exlo.parsers.ActionParser as AP
import acab.modules.parsing.exlo.parsers.FactParser as FP
import acab.modules.parsing.exlo.parsers.QueryParser as QP
import acab.modules.parsing.exlo.parsers.RuleParser as RP
import acab.modules.parsing.exlo.parsers.TransformParser as TP
import acab.modules.printing.printers as Printers
from acab.abstract.config.config import AcabConfig
from acab.abstract.core import default_structure as DS
from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator)
from acab.abstract.core.values import AcabStatement, AcabValue, Sentence
from acab.abstract.interfaces.handler_system import Handler
from acab.modules.printing.basic_printer import BasicPrinter

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

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

        FP.HOTLOAD_ANNOTATIONS << pp.MatchFirst([QP.word_query_constraint])

        FP.HOTLOAD_SEN_ENDS << pp.MatchFirst([QP.query_sen_end,
                                              QP.query_statement,
                                              TP.transform_statement,
                                              AP.action_definition])


    def test_sentence_basic(self):
        sem = BasicPrinter(init_handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                          Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                          Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                          Printers.ModalPrinter().as_handler("_:MODAL")],
                           settings={"MODAL" : ""})
        words = ["a", "b", "c", "d"]
        sentence = Sentence.build(words)

        result = sem.pprint(sentence)
        self.assertEqual(result, FALLBACK_MODAL_S.join(words))

    def test_sentence_modal(self):
        sem = BasicPrinter(init_handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                          Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                          Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                          Printers.ModalPrinter().as_handler("_:MODAL")],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test.sen")

    def test_sentence_modal_2(self):
        sem = BasicPrinter(init_handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                          Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                          Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                          Printers.ModalPrinter().as_handler("_:MODAL")],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen")

    def test_sentence_query(self):
        sem = BasicPrinter(init_handlers=[Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                          Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                          Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                          Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                          Printers.ModalPrinter().as_handler("_:MODAL")],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        sentence[-1].data[DS.QUERY] = True
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen?")

    def test_sentence_negated(self):
        sem = BasicPrinter(init_handlers=[Printers.ModalAwarePrinter().as_handler("_:ATOM"),
                                          Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                          Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                          Printers.ModalPrinter().as_handler("_:MODAL")],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("~a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "~a.test.sen")
