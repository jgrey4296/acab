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
import acab.modules.parsing.exlo.parsers.FactParser as FP

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



class PrintValueSemanticTests(unittest.TestCase):
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
        sem_sys = PrintSemanticSystem(handlers=[Printers.BasicPrinter("_:ATOM")])
        result = sem_sys.pprint(AcabValue("Test"))
        self.assertEqual(result, "Test")

    def test_multiple(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.BasicPrinter("_:ATOM")])
        result = sem_sys.pprint(AcabValue("a"), AcabValue("b"), AcabValue("c"))
        self.assertEqual(result, r"a\nb\nc")


    def test_string_wrap(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.PrimitiveTypeAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL")])

        test = AcabValue(name="blah", data={TYPE_INSTANCE_S: STR_PRIM_S})
        result = sem_sys.pprint(test)
        self.assertEqual(result, '"blah"')

    def test_regex_wrap(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.PrimitiveTypeAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL")])
        test = AcabValue(value=re.compile("blah"), data={TYPE_INSTANCE_S: REGEX_PRIM_S})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'/blah/')

    def test_var_wrap(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.PrimitiveTypeAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL")])
        test = AcabValue("blah", data={BIND_S : True})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'$blah')

    def test_pprint_at_var(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.PrimitiveTypeAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL")])
        value = AcabValue("test")
        value.data.update({BIND_S: AT_BIND_S})
        result = sem_sys.pprint(value)
        self.assertEqual(result, r'@test')

    def test_modal_print(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL")])
        test = AcabValue(value=re.compile("blah"),
                         data={'exop': config.defaults['exop']})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'blah.')

    def test_modal_print2(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL")])

        test = AcabValue(value=re.compile("blah"),
                         data={'exop': config.enums['exop'].EX})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'blah!')

    def test_modal_print_override(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL",
                                                                                   override={("exop", "DOT") : "^"})])
        test = AcabValue(value=re.compile("blah"),
                         data={'exop': config.enums['exop'].DOT})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'blah^')

    def test_symbol_override(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL",
                                                                                   override={("Symbols", "BIND") : "%"})])
        test = AcabValue(value=re.compile("blah"), data={BIND_S : True})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'%blah')

    def test_value_uuid(self):
        val = AcabValue("test")
        sem_sys = PrintSemanticSystem(handlers=[Printers.UUIDAwarePrinter("_:ATOM")])
        result = sem_sys.pprint(val)
        self.assertEqual(result, "({} : {})".format(val.name, val.uuid))


    def test_constraints(self):
        sem_sys = PrintSemanticSystem(handlers=[Printers.ModalAwarePrinter("_:ATOM"),
                                                Printers.BasicSentenceAwarePrinter("_:SENTENCE"),
                                                Printers.ConstraintAwareValuePrinter("_:CONSTRAINT"),
                                                Printers.ConfigBackedSymbolPrinter("_:SYMBOL",
                                                                                   override={("Symbols", "BIND") : "%"})])
        value = FP.parseString("con.test(λa.test.op $x)")[0][-1]
        result = sem_sys.pprint(value)
        self.assertEqual(result, "test(λa.test.op $x).")
