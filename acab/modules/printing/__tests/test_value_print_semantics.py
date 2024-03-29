#!/usr/bin/env python3

import logging as logmod
import re
import unittest
import unittest.mock as mock
from enum import Enum
from os.path import split, splitext

import pyparsing as pp

logging = logmod.getLogger(__name__)
##############################

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()


    import acab.core.defaults.value_keys as DS
    import acab.modules.parsing.exlo.parsers.ActionParser as AP
    import acab.modules.parsing.exlo.parsers.FactParser as FP
    import acab.modules.parsing.exlo.parsers.QueryParser as QP
    import acab.modules.parsing.exlo.parsers.RuleParser as RP
    import acab.modules.parsing.exlo.parsers.TransformParser as TP
    import acab.modules.printing.printers as Printers
    from acab import AcabConfig
    from acab.core.defaults import print_signals as DSig
    from acab.core.value.instruction import (Instruction, ProductionContainer,
                                            ProductionOperator)
    from acab.core.util.sentences import ProductionComponent

    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.handler_system import Handler_i
    from acab.modules.printing import default
    from acab.modules.printing.basic_printer import BasicPrinter

QUERY_S           = DS.QUERY
BIND_S            = DS.BIND
AT_BIND_S         = DS.AT_BIND
TYPE_INSTANCE_S   = DS.TYPE_INSTANCE

SEN_JOIN_S        = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])()

STR_PRIM_S        = Sentence([config.prepare("Type.Primitive", "STRING")()])
REGEX_PRIM_S      = Sentence([config.prepare("Type.Primitive", "REGEX")()])

EXOP              = config.prepare("exop", _type=Enum)()
DOT_E             = EXOP.DOT


class PrintValueSemanticTests(unittest.TestCase):
    """ Test the basic Print Semantics using default settings """

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

    @classmethod
    def tearDownClass(cls):
        logging.root.removeHandler(cls.file_h)

    def setUp(self):
        FP.HOTLOAD_ANNOTATIONS << pp.MatchFirst([QP.word_query_constraint])
        FP.HOTLOAD_SEN_ENDS    << pp.MatchFirst([QP.query_statement,
                                              TP.transform_statement,
                                              AP.action_definition])
        FP.HOTLOAD_SEN_POSTS   << QP.query_sen_post_annotation

    def tearDown(self):
        FP.HOTLOAD_ANNOTATIONS << pp.NoMatch()
        FP.HOTLOAD_SEN_ENDS    << pp.NoMatch()
        FP.HOTLOAD_SEN_POSTS   << pp.NoMatch()


    def test_initial(self):
        """ Check a basic value can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.AtomicPrinter().as_handler(signal="[ATOM]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")]
                               )
        result = sem_sys.pprint(AcabValue("Test"))
        self.assertEqual(result, "Test")

    def test_multiple(self):
        """ Check multiple values can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.AtomicPrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               )
        result = sem_sys.pprint(AcabValue("a"), AcabValue("b"), AcabValue("c"))
        self.assertEqual(result, "a\nb\nc")


    def test_string_wrap(self):
        """ Check a string type value can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.PrimitiveTypeAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")]
                               )


        test = AcabValue(name="blah", data={TYPE_INSTANCE_S: STR_PRIM_S})
        result = sem_sys.pprint(test)
        self.assertEqual(result, '"blah"')

    def test_regex_wrap(self):
        """ Check a regex type value can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.PrimitiveTypeAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               )
        test = AcabValue(value=re.compile("blah"), data={TYPE_INSTANCE_S: REGEX_PRIM_S})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'/blah/')

    def test_var_wrap(self):
        """ Check a variable type value can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.PrimitiveTypeAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               )
        test = AcabValue("blah", data={BIND_S : True})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'$blah')

    def test_pprint_at_var(self):
        """ Check an @ variable can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.PrimitiveTypeAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               )
        value = AcabValue("test")
        value.data.update({BIND_S: AT_BIND_S})
        result = sem_sys.pprint(value)
        self.assertEqual(result, r'@test')

    def test_modal_print(self):
        """ Check a modal can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.ModalAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")
                                              ],
                               settings={"MODAL" : "exop"})
        test = AcabValue(value="blah",
                         data={'exop': config.default("exop")})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'blah.')

    def test_modal_print2(self):
        """ Check the other modal can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.ModalAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")
                                              ],
                               settings={"MODAL" : "exop"})

        test = AcabValue(value="blah",
                         data={'exop': EXOP.EX})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'blah!')

    def test_modal_print_override(self):
        """ Check modal symbols can be overriden for printing """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.ModalAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter(overrides={DOT_E : "^"}).as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")
                                              ],
                               settings={"MODAL": "exop"})
        test = AcabValue(value="blah",
                         data={'exop': DOT_E})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'blah^')

    def test_symbol_override(self):
        """ Check bind symbols can be overriden for printing """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.ModalAwarePrinter().as_handler(signal="[ATOM]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConfigBackedSymbolPrinter(overrides={config.prepare("Symbols", "BIND") : "%"}).as_handler(signal="SYMBOL"),
                                              Printers.NoOpPrinter().as_handler(signal="MODAL")
                                              ],
                               )
        test = AcabValue(value="blah",
                         data={BIND_S : True})
        result = sem_sys.pprint(test)
        self.assertEqual(result, r'%blah')

    def test_value_uuid(self):
        """ Check the uuid of a value can be printed """
        val = AcabValue("test")
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.UUIDAwarePrinter().as_handler(signal="[ATOM]")],
                               )
        result = sem_sys.pprint(val)
        self.assertEqual(result, "({} : {})".format(val.uuid, val.name))


    def test_constraints(self):
        """ Check constraints of values can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[
                                   Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                   Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                   Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                   Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                   Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                   Printers.ConstraintPrinter().as_handler(signal=DSig.CONSTRAINT),
                                   Printers.ModalPrinter().as_handler(signal="MODAL"),
                                   Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                   Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                               ],
                               settings={"MODAL": "exop"})
        value = FP.parse_string("test(λa.test.op $x)")[0][-1]
        result = sem_sys.pprint(value)
        self.assertEqual(result, "test(λa.test.op $x).")

    def test_constraints_multi_var(self):
        """ Check constraints with multiple variables can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.SimpleTypePrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.ConstraintPrinter().as_handler(signal="CONSTRAINT"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL)],
                               settings={"MODAL": "exop"})
        value = FP.parse_string("con.test(λa.test.op $x $y)")[0][-1]
        result = sem_sys.pprint(value)
        self.assertEqual(result, "test(λa.test.op $x $y).")
