#!/usr/bin/env python3
import logging as logmod
import re
import unittest
import unittest.mock as mock
from enum import Enum
from os.path import split, splitext

logging = logmod.getLogger(__name__)
import warnings

import acab
##############################
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    # from acab.core.parsing import debug_funcs as DBF
    # DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)

    import acab.modules.printing.printers as Printers
    from acab.core.config.config import AcabConfig
    from acab.core.defaults import print_signals as DSig
    from acab.core.defaults.value_keys import (AT_BIND, BIND, NEGATION, QUERY,
                                            TYPE_INSTANCE)
    from acab.core.parsing import pyparse_dsl as ppDSL
    from acab.core.util.sentences import ProductionComponent
    from acab.core.value.instruction import (Instruction, ProductionContainer,
                                            ProductionOperator,
                                            ProductionStructure)
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.interfaces.handler_system import Handler_i
    from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
    from acab.modules.parsing.exlo.parsers import FactParser as FP
    from acab.modules.parsing.exlo.parsers import QueryParser as QP
    from acab.modules.printing import default
    from acab.modules.printing.basic_printer import BasicPrinter

NEGATION_SYMBOL_S = config.prepare("Symbols", "NEGATION")()
ANON_VALUE_S      = config.prepare("Symbols", "ANON_VALUE")()
FALLBACK_MODAL_S  = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()
QUERY_SYMBOL_S    = config.prepare("Symbols", "QUERY")()

SEN_JOIN_S        = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])()

STR_PRIM_S        = Sentence([config.prepare("Type.Primitive", "STRING")()])
REGEX_PRIM_S      = Sentence([config.prepare("Type.Primitive", "REGEX")()])

EXOP              = config.prepare("exop", _type=Enum)()
DOT_E             = EXOP.DOT

dsl = None

class PrintStructureSemanticTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.handlers[0].setLevel(logmod.WARNING)
        logging.root.addHandler(cls.file_h)

        global dsl
        # Set up the parser to ease test setup
        dsl   = ppDSL.PyParseDSL()
        dsl.register(EXLO_Parser)
        dsl.build()
        # dsl()

    @classmethod
    def tearDownClass(cls):
        logging.root.removeHandler(cls.file_h)


    def test_component_simple(self):
        """ Check production components can be printed """
        component = ProductionComponent(dsl("testop.blah")[0])
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               settings={"MODAL": "exop"})
        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah")

    def test_component_simple2(self):
        """ Check production components with variables can be printed """
        component = ProductionComponent(dsl("testop.blah")[0],
                                        params=[dsl("$x")[0][0]])
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               settings={"MODAL": "exop"})
        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah $x")

    def test_transform_simple(self):
        """ Check transforms print the rebind variable """
        component = ProductionComponent(dsl("testop.blah")[0],
                                        params=[dsl("$x")[0][0]],
                                        rebind=dsl("$y")[0][0])
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               settings={"MODAL": "exop"})

        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah $x -> $y")

    def test_implicit_container(self):
        """ Check containers print all their clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ConstraintPrinter().as_handler(signal="CONSTRAINT"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ImplicitContainerPrinter().as_handler(signal="[INSTRUCT.CONTAINER]"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               settings={"MODAL": "exop"})

        # combine some queries together
        queries = QP.clauses.parse_string(" a.b.c?\n d.e(λa.b.q $y).f?\n g.h.i?")[0]
        self.assertIsInstance(queries, ProductionContainer)
        result = sem_sys.pprint(queries)

        self.assertEqual(result, "    a.b.c?\n    d.e(λa.b.q $y).f?\n    g.h.i?\n")


    def test_rule(self):
        """ Check a rule can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ImplicitContainerPrinter().as_handler(signal="IMPLICIT_CONTAINER"),
                                              Printers.StructurePrinter().as_handler(signal="[INSTRUCT.STRUCTURE]"),
                                              Printers.TagPrinter().as_handler(signal="TAGS"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               settings={"MODAL": "exop"})

        # parse a rule
        rule = dsl("rule(::ρ):\na.b.c?\n\nλa.b.c\nend")[0][-1]
        self.assertIsInstance(rule, ProductionStructure)
        # print
        result = sem_sys.pprint(rule)
        self.assertEqual(result, "rule(::ρ):\n    a.b.c?\n\n    λa.b.c\nend\n")

    def test_rule_with_param(self):
        """ Check a rule can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ImplicitContainerPrinter().as_handler(signal="IMPLICIT_CONTAINER"),
                                              Printers.StructurePrinter().as_handler(signal="[INSTRUCT.STRUCTURE]"),
                                              Printers.TagPrinter().as_handler(signal="TAGS"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")],
                               settings={"MODAL": "exop"})

        # parse a rule
        rule = dsl("rule(::ρ):\n | $x |\n\n a.b.c?\n\nλa.b.c\nend")[0][-1]
        self.assertIsInstance(rule, ProductionStructure)
        # print
        result = sem_sys.pprint(rule)
        self.assertEqual(result, "rule(::ρ):\n    | $x |\n\n    a.b.c?\n\n    λa.b.c\nend\n")


    def test_rule_with_tags(self):
        """ Check a rule prints its tags """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ImplicitContainerPrinter().as_handler(signal="IMPLICIT_CONTAINER"),
                                              Printers.StructurePrinter().as_handler(signal="[INSTRUCT.STRUCTURE]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.TagPrinter().as_handler(signal="TAGS"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              ],
                              settings={"MODAL": "exop"})

        # parse a rule
        rule = dsl("""rule(::ρ):
        #test.tag

        a.b.c?

        λa.b.c
        end""")[0][-1]
        self.assertIsInstance(rule, ProductionStructure)
        # print
        result = sem_sys.pprint(rule)
        self.assertEqual(result, "rule(::ρ):\n    #tag.test\n\n    a.b.c?\n\n    λa.b.c\nend\n")


    def test_query_statement(self):
        """ Check a query statement prints its clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ExplicitContainerPrinter().as_handler(signal="[INSTRUCT.CONTAINER]"),
                                              Printers.StructurePrinter().as_handler(signal="[INSTRUCT.STRUCTURE]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              ],
                              settings={"MODAL": "exop"})

        query = dsl("""statement(::γ):
        a.b.c?
        d.e.f?
        g.h.e?

        end
        """)[0][-1]

        self.assertIsInstance(query, ProductionContainer)
        result = sem_sys.pprint(query)
        self.assertEqual(result, """statement(::γ):\n    a.b.c?\n    d.e.f?\n    g.h.e?\nend\n""")

    def test_transform_statement(self):
        """ Check a transform statement prints its clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ExplicitContainerPrinter().as_handler(signal="[INSTRUCT.CONTAINER]"),
                                              Printers.StructurePrinter().as_handler(signal="[INSTRUCT.STRUCTURE]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              ],
                               settings={"MODAL": "exop"})

        query = dsl("""statement(::χ):
        λa.b.c $x $y -> $z
        λq.c.d $z $x -> $a
        λa.b.c $a $y -> $c

        end
        """)[0][-1]

        self.assertIsInstance(query, ProductionContainer)
        result = sem_sys.pprint(query)
        self.assertEqual(result, """statement(::χ):\n    λa.b.c $x $y -> $z\n    λq.c.d $z $x -> $a\n    λa.b.c $a $y -> $c\nend\n""")

    def test_action_statement(self):
        """ Check an action statement prints its clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler(signal="[SENTENCE]"),
                                              Printers.AnnotationAwareValuePrinter().as_handler(signal="[ATOM]"),
                                              Printers.AnnotationPrinter().as_handler(signal="ANNOTATIONS"),
                                              Printers.AnnotationFinaliser().as_handler(signal=DSig.ANNOTATIONS_FINAL),
                                              Printers.ProductionComponentPrinter().as_handler(signal="[SENTENCE.COMPONENT]"),
                                              Printers.TypeAliasPrinter().as_handler(signal="TYPE_INSTANCE"),
                                              Printers.ExplicitContainerPrinter().as_handler(signal="[INSTRUCT.CONTAINER]"),
                                              Printers.ImplicitContainerPrinter().as_handler(signal="IMPLICIT_CONTAINER"),
                                              Printers.StructurePrinter().as_handler(signal="[INSTRUCT.STRUCTURE]"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler(signal="SYMBOL"),
                                              Printers.ModalPrinter().as_handler(signal="MODAL")
                                              ],
                               settings={"MODAL": "exop"})
        action = dsl("""statement(::α):\n λa.b.c $x\n λa.b.c.d $x $y\n λa.b.c.d.e $x $y $z\nend""")[0][-1]
        self.assertIsInstance(action, ProductionContainer)
        result = sem_sys.pprint(action)
        self.assertEqual(result, """statement(::α):\n    λa.b.c $x\n    λa.b.c.d $x $y\n    λa.b.c.d.e $x $y $z\nend\n""")
