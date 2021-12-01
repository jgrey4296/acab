#!/usr/bin/env python3


import logging as root_logger
import re
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = root_logger.getLogger(__name__)
import acab
##############################
import pyparsing as pp

config = acab.setup()

import acab.modules.parsing.exlo.parsers.ActionParser as AP
import acab.modules.parsing.exlo.parsers.FactParser as FP
import acab.modules.parsing.exlo.parsers.QueryParser as QP
import acab.modules.parsing.exlo.parsers.RuleParser as RP
import acab.modules.parsing.exlo.parsers.TransformParser as TP
import acab.modules.printing.printers as Printers
from acab.core.config.config import AcabConfig
from acab.core.data.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionOperator,
                                                        ProductionStructure)
from acab.core.data.values import AcabStatement, AcabValue, Sentence
from acab.interfaces.handler_system import Handler
from acab.modules.printing.basic_printer import BasicPrinter
from acab.modules.printing import default

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



    def test_component_simple(self):
        """ Check production components can be printed """
        component = ProductionComponent(value=FP.parseString("testop.blah")[0])
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              Printers.ModalPrinter().as_handler("_:MODAL")],
                               settings={"MODAL": "exop"})

        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah")

    def test_component_simple2(self):
        """ Check production components with variables can be printed """
        component = ProductionComponent(value=FP.parseString("testop.blah")[0],
                                        params=[FP.parseString("$x")[0][0]])
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              Printers.ModalPrinter().as_handler("_:MODAL")],
                               settings={"MODAL": "exop"})

        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah $x")

    def test_transform_simple(self):
        """ Check transforms print the rebind variable """
        component = ProductionComponent(value=FP.parseString("testop.blah")[0],
                                        params=[FP.parseString("$x")[0][0]],
                                        rebind=FP.parseString("$y")[0][0])
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              Printers.ModalPrinter().as_handler("_:MODAL")],
                               settings={"MODAL": "exop"})

        result = sem_sys.pprint(component)
        self.assertEqual(result, "λtestop.blah $x -> $y")

    def test_container(self):
        """ Check containers print all their clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.ConstraintPrinter().as_handler("_:CONSTRAINT"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.ImplicitContainerPrinter().as_handler("_:CONTAINER"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              Printers.ModalPrinter().as_handler("_:MODAL")],
                               settings={"MODAL": "exop"})

        # combine some queries together
        queries = QP.parseString("a.b.c?\nd.e(λa.b.q $y).f?\ng.h.i?")
        self.assertIsInstance(queries, ProductionContainer)

        result = sem_sys.pprint(queries)

        self.assertEqual(result, "    a.b.c?\n    d.e(λa.b.q $y).f?\n    g.h.i?\n")


    def test_rule(self):
        """ Check a rule can be printed """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.ImplicitContainerPrinter().as_handler("_:IMPLICIT_CONTAINER"),
                                              Printers.StructurePrinter().as_handler("_:STRUCTURE"),
                                              Printers.TagPrinter().as_handler("_:TAGS"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              Printers.ModalPrinter().as_handler("_:MODAL")],
                               settings={"MODAL": "exop"})

        # parse a rule
        rule = RP.parseString("rule(::ρ):\na.b.c?\n\nλa.b.c\nend")[0]
        self.assertIsInstance(rule, ProductionStructure)
        # print
        result = sem_sys.pprint(rule)
        self.assertEqual(result, "rule(::_:RULE):\n    a.b.c?\n\n    λa.b.c\nend\n")

    def test_rule_with_tags(self):
        """ Check a rule prints its tags """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.ImplicitContainerPrinter().as_handler("_:IMPLICIT_CONTAINER"),
                                              Printers.StructurePrinter().as_handler("_:STRUCTURE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.TagPrinter().as_handler("_:TAGS"),
                                              Printers.ModalPrinter().as_handler("_:MODAL"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS")
                                              ],
                              settings={"MODAL": "exop"})

        # parse a rule
        rule = RP.parseString("""rule(::ρ):
        #test.tag

        a.b.c?

        λa.b.c
        end""")[0]
        self.assertIsInstance(rule, ProductionStructure)
        # print
        result = sem_sys.pprint(rule)
        self.assertEqual(result, "rule(::_:RULE):\n    #tag.test\n\n    a.b.c?\n\n    λa.b.c\nend\n")


    def test_query_statement(self):
        """ Check a query statement prints its clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.ExplicitContainerPrinter().as_handler("_:CONTAINER"),
                                              Printers.StructurePrinter().as_handler("_:STRUCTURE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.ModalPrinter().as_handler("_:MODAL"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              ],
                              settings={"MODAL": "exop"})

        query = QP.query_statement.parseString("""statement(::γ):
        a.b.c?
        d.e.f?
        g.h.e?

        end
        """)[0]

        self.assertIsInstance(query, ProductionContainer)
        result = sem_sys.pprint(query)
        self.assertEqual(result, """statement(::_:QUERY):\n    a.b.c?\n    d.e.f?\n    g.h.e?\nend\n""")

    def test_transform_statement(self):
        """ Check a transform statement prints its clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.ExplicitContainerPrinter().as_handler("_:CONTAINER"),
                                              Printers.StructurePrinter().as_handler("_:STRUCTURE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.ModalPrinter().as_handler("_:MODAL"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              ],
                               settings={"MODAL": "exop"})

        query = TP.transform_statement.parseString("""statement(::χ):
        λa.b.c $x $y -> $z
        λq.c.d $z $x -> $a
        λa.b.c $a $y -> $c

        end
        """)[0]

        self.assertIsInstance(query, ProductionContainer)
        result = sem_sys.pprint(query)
        self.assertEqual(result, """statement(::_:TRANSFORM):\n    λa.b.c $x $y -> $z\n    λq.c.d $z $x -> $a\n    λa.b.c $a $y -> $c\nend\n""")

    def test_action_statement(self):
        """ Check an action statement prints its clauses """
        sem_sys = BasicPrinter(init_specs=default.DEFAULT_PRINTER_SPEC(),
                               init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
                                              Printers.ExplicitContainerPrinter().as_handler("_:CONTAINER"),
                                              Printers.ImplicitContainerPrinter().as_handler("_:IMPLICIT_CONTAINER"),
                                              Printers.StructurePrinter().as_handler("_:STRUCTURE"),
                                              Printers.ConfigBackedSymbolPrinter().as_handler("_:SYMBOL"),
                                              Printers.AnnotationPrinter().as_handler("_:ANNOTATIONS"),
                                              Printers.ModalPrinter().as_handler("_:MODAL")
                                              ],
                               settings={"MODAL": "exop"})

        action = AP.action_definition.parseString("""statement(::α):
        λa.b.c $x
        λa.b.c.d $x $y
        λa.b.c.d.e $x $y $z
        end
        """)[0]

        self.assertIsInstance(action, ProductionContainer)

        result = sem_sys.pprint(action)
        self.assertEqual(result, """statement(::_:ACTION):\n    λa.b.c $x\n    λa.b.c.d $x $y\n    λa.b.c.d.e $x $y $z\nend\n""")
