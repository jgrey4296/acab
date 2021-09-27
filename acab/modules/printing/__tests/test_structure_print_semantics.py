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
import acab.modules.parsing.exlo.parsers.QueryParser as QP
import acab.modules.parsing.exlo.parsers.RuleParser as RP
import acab.modules.parsing.exlo.parsers.TransformParser as TP
import acab.modules.parsing.exlo.parsers.ActionParser as AP
import acab.modules.printing.printers as Printers
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionStructure,
                                                        ProductionOperator)
from acab.abstract.core.values import AcabStatement, AcabValue, Sentence
from acab.modules.printing.basic_printer import BasicPrinter
from acab.abstract.interfaces.handler_system import Handler

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
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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
        component = ProductionComponent(value=FP.parseString("testop.blah")[0],
                                        params=[FP.parseString("$x")[0][0]])
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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
        component = ProductionComponent(value=FP.parseString("testop.blah")[0],
                                        params=[FP.parseString("$x")[0][0]],
                                        rebind=FP.parseString("$y")[0][0])
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
                                              Printers.AnnotationAwareValuePrinter().as_handler("_:ATOM"),
                                              Printers.ProductionComponentPrinter().as_handler("_:COMPONENT"),
                                              Printers.SimpleTypePrinter().as_handler("_:TYPE_INSTANCE"),
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
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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
        rule = RP.parseString("a.test.rule:\na.b.c?\n\nλa.b.c\nend")[0]
        self.assertIsInstance(rule, Sentence)
        self.assertIsInstance(rule[-1], ProductionStructure)
        # print
        result = sem_sys.pprint(rule)
        self.assertEqual(result, "a.test.rule(::_:RULE):\n    a.b.c?\n\n    λa.b.c\nend\n")

    def test_rule_with_tags(self):
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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
        rule = RP.parseString("""a.test.rule:
        #test.tag

        a.b.c?

        λa.b.c
        end""")[0]
        self.assertIsInstance(rule, Sentence)
        self.assertIsInstance(rule[-1], ProductionStructure)
        # print
        result = sem_sys.pprint(rule)
        self.assertEqual(result, "a.test.rule(::_:RULE):\n    #tag.test\n\n    a.b.c?\n\n    λa.b.c\nend\n")


    def test_query_statement(self):
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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

        query = QP.query_statement.parseString("""test.statement:
        a.b.c?
        d.e.f?
        g.h.e?

        end
        """)[0]

        self.assertIsInstance(query[-1], ProductionContainer)
        result = sem_sys.pprint(query)
        self.assertEqual(result, """test.statement(::_:QUERY):\n    a.b.c?\n    d.e.f?\n    g.h.e?\nend\n""")

    def test_transform_statement(self):
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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

        query = TP.transform_statement.parseString("""test.statement:
        λa.b.c $x $y -> $z
        λq.c.d $z $x -> $a
        λa.b.c $a $y -> $c

        end
        """)[0]

        self.assertIsInstance(query[-1], ProductionContainer)
        result = sem_sys.pprint(query)
        self.assertEqual(result, """test.statement(::_:TRANSFORM):\n    λa.b.c $x $y -> $z\n    λq.c.d $z $x -> $a\n    λa.b.c $a $y -> $c\nend\n""")

    def test_action_statement(self):
        sem_sys = BasicPrinter(init_handlers=[Printers.BasicSentenceAwarePrinter().as_handler("_:SENTENCE"),
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

        query = AP.action_definition.parseString("""test.statement:
        λa.b.c $x
        λa.b.c.d $x $y
        λa.b.c.d.e $x $y $z
        end
        """)[0]

        self.assertIsInstance(query[-1], ProductionContainer)

        result = sem_sys.pprint(query)
        self.assertEqual(result, """test.statement(::_:ACTION):\n    λa.b.c $x\n    λa.b.c.d $x $y\n    λa.b.c.d.e $x $y $z\nend\n""")
