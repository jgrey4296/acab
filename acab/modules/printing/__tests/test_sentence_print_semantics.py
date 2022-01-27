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
from acab.modules.printing import default
from acab.core.config.config import AcabConfig
from acab.core.data import default_structure as DS
from acab.core.data.instruction import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator)
from acab.core.data.value import Instruction, AcabValue, Sentence
from acab.interfaces.handler_system import Handler
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
        """ Check a sentence can be printed """
        init_handlers = [Printers.ModalAwarePrinter().as_handler(signal="ATOM"),
                         Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                         Printers.BasicSentenceAwarePrinter().as_handler(signal="SENTENCE"),
                         Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                         Printers.ModalPrinter().as_handler(signal="MODAL")]

        specs = default.DEFAULT_PRINTER_SPEC()
        sem = BasicPrinter(init_specs=specs,
                           init_handlers=init_handlers)

        words = ["a", "b", "c", "d"]
        sentence = Sentence.build(words)

        result = sem.pprint(sentence)
        self.assertEqual(result, FALLBACK_MODAL_S.join(words))

    def test_sentence_modal(self):
        """ Check a modal sentence can be printed """
        sem = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                           init_handlers=[Printers.ModalAwarePrinter().as_handler(signal="ATOM"),
                                          Printers.BasicSentenceAwarePrinter().as_handler(signal="SENTENCE"),
                                          Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                          Printers.ModalPrinter().as_handler(signal="MODAL")],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test.sen")

    def test_sentence_modal_2(self):
        """ Check a sentence with different modal values can be printed """
        sem = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                           init_handlers=[Printers.ModalAwarePrinter().as_handler(signal="ATOM"),
                                          Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                          Printers.BasicSentenceAwarePrinter().as_handler(signal="SENTENCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                          Printers.ModalPrinter().as_handler(signal="MODAL")],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen")

    def test_sentence_query(self):
        sem = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                           init_handlers=[Printers.AnnotationAwareValuePrinter().as_handler(signal="ATOM"),
                                          Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                          Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                          Printers.BasicSentenceAwarePrinter().as_handler(signal="SENTENCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                          Printers.ModalPrinter().as_handler(signal="MODAL")],
                          settings={"MODAL": "exop"})
        sentence = FP.parseString("a.test!sen")[0]
        sentence[-1].data[DS.QUERY] = True
        result = sem.pprint(sentence)
        self.assertEqual(result, "a.test!sen?")

    def test_sentence_negated(self):
        """ Check a negated sentence can be printed """
        sem = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                           init_handlers=[Printers.ModalAwarePrinter().as_handler(signal="ATOM"),
                                          Printers.BasicSentenceAwarePrinter().as_handler(signal="SENTENCE"),
                                          Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                          Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                          Printers.ModalPrinter().as_handler(signal="MODAL")],
                          settings={"MODAL" : "exop"})
        sentence = FP.parseString("~a.test.sen")[0]
        result = sem.pprint(sentence)
        self.assertEqual(result, "~a.test.sen")
