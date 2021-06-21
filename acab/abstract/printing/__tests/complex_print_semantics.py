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

import acab.modules.semantics.printers as Printers
from acab.abstract.printing.print_types import RET_enum
from acab.abstract.printing import default_handlers as DH

import acab.modules.parsing.exlo.FactParser as FP

NEGATION_S        = config.value("Value.Structure", "NEGATION")
QUERY_S           = config.value("Value.Structure", "QUERY")
BIND_S            = config.value("Value.Structure", "BIND")
AT_BIND_S           = config.value("Value.Structure", "AT_BIND")

NEGATION_SYMBOL_S = config.value("Symbols", "NEGATION")
ANON_VALUE_S      = config.value("Symbols", "ANON_VALUE")
FALLBACK_MODAL_S  = config.value("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])
QUERY_SYMBOL_S    = config.value("Symbols", "QUERY")

SEN_JOIN_S       = config.value("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])

STR_PRIM_S       = Sentence.build([config.value("Type.Primitive", "STRING")])
REGEX_PRIM_S     = Sentence.build([config.value("Type.Primitive", "REGEX")])
TYPE_INSTANCE_S  = config.value("Value.Structure", "TYPE_INSTANCE")

class PrintComplexSentenceSemanticTests(unittest.TestCase):
    @unittest.skip
    def test_sentence_negated(self):
        join_str = "."
        sem = BasicPrinter(basic_plus, {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["a","b","c","d"])
        sentence.data[NEGATION_S] = True

        result = sem.pprint(sentence)
        self.assertEqual(result, "{}{}".format(NEGATION_SYMBOL_S,
                                               join_str.join(["a", "b", "c", "d"])))

    @unittest.skip
    def test_sentence_query(self):
        join_str = "."
        sem = BasicPrinter(basic_plus, {SEN_JOIN_S: join_str})
        sentence = Sentence.build(["a","b","c","d"])
        sentence.data[QUERY_S] = True

        result = sem.pprint(sentence)
        self.assertEqual(result, "{}{}".format(join_str.join(["a", "b", "c", "d"]),
                                               QUERY_SYMBOL_S))

    @unittest.skip
    def test_type_instance(self):
        instance = Sentence.build(["a","b","c"])
        sem = BasicPrinter(basic_plus)

        # TODO test type instance

    # drop end op, constraints

    def test_transform(self):
        pass

    def test_operator_path(self):
        pass
