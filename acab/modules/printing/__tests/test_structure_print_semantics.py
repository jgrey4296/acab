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
from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator)
from acab.abstract.core.values import AcabStatement, AcabValue, Sentence
from acab.abstract.interfaces.printing_interfaces import PrintSemanticSystem

NEGATION_S        = config.prepare("Value.Structure", "NEGATION")()
QUERY_S           = config.prepare("Value.Structure", "QUERY")()
BIND_S            = config.prepare("Value.Structure", "BIND")()
AT_BIND_S         = config.prepare("Value.Structure", "AT_BIND")()
TYPE_INSTANCE_S   = config.prepare("Value.Structure", "TYPE_INSTANCE")()

NEGATION_SYMBOL_S = config.prepare("Symbols", "NEGATION")()
ANON_VALUE_S      = config.prepare("Symbols", "ANON_VALUE")()
FALLBACK_MODAL_S  = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()
QUERY_SYMBOL_S    = config.prepare("Symbols", "QUERY")()

SEN_JOIN_S        = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])()

STR_PRIM_S        = Sentence.build([config.prepare("Type.Primitive", "STRING")()])
REGEX_PRIM_S      = Sentence.build([config.prepare("Type.Primitive", "REGEX")()])

EXOP              = config.prepare("exop", as_enum=True)()
DOT_E             = EXOP.DOT

class PrintStructureSemanticTests(unittest.TestCase):
    def test_component_simple(self):
        component = ProductionComponent(value=FP.parseString("testop.blah")[0])
        sem_sys = PrintSemanticSystem(handlers=[Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                                Printers.ConstraintAwareValuePrinter("_:ATOM"),
                                                Printers.ProductionComponentPrinter("_:COMPONENT"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                                Printers.PrimitiveTypeAwarePrinter("_:OVERRIDE_VAR")],
                                      settings={"MODAL": "exop"})

        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah")

    def test_component_simple2(self):
        component = ProductionComponent(value=FP.parseString("testop.blah")[0],
                                        params=[FP.parseString("$x")[0][0]])
        sem_sys = PrintSemanticSystem(handlers=[Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                                Printers.ConstraintAwareValuePrinter("_:ATOM"),
                                                Printers.ProductionComponentPrinter("_:COMPONENT"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                                Printers.PrimitiveTypeAwarePrinter("_:OVERRIDE_VAR")],
                                      settings={"MODAL": "exop"})

        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah $x")

    def test_transform_simple(self):
        component = ProductionComponent(value=FP.parseString("testop.blah")[0],
                                        params=[FP.parseString("$x")[0][0]],
                                        rebind=FP.parseString("$y")[0][0])
        sem_sys = PrintSemanticSystem(handlers=[Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                                Printers.ConstraintAwareValuePrinter("_:ATOM"),
                                                Printers.ProductionComponentPrinter("_:COMPONENT"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                                Printers.PrimitiveTypeAwarePrinter("_:OVERRIDE_VAR")],
                                      settings={"MODAL": "exop"})

        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah $x -> $y")

    def test_container(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                                Printers.ConstraintAwareValuePrinter("_:ATOM"),
                                                Printers.ProductionComponentPrinter("_:COMPONENT"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                                Printers.PrimitiveTypeAwarePrinter("_:OVERRIDE_VAR")],
                                      settings={"MODAL": "exop"})

        # combine some queries together

        # print
        pass

    def test_rule(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                                Printers.ConstraintAwareValuePrinter("_:ATOM"),
                                                Printers.ProductionComponentPrinter("_:COMPONENT"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL"),
                                                Printers.PrimitiveTypeAwarePrinter("_:OVERRIDE_VAR")],
                                      settings={"MODAL": "exop"})

        # parse a rule

        # print

        pass
