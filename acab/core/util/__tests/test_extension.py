from __future__ import annotations

import logging as logmod
import unittest
import warnings
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
from unittest import mock

import acab
import pyparsing as pp

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)


import acab.core.defaults.value_keys as DS
from acab.core.defaults import print_signals as DSig
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.parsing.component_dsl import Component_DSL
from acab.core.semantics.basic import StatementSemantics
from acab.core.util import fragments as FR
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.interfaces import fragments as FI
from acab.interfaces import handler_system as HS
from acab.interfaces.semantic import StatementSemantics_i
from acab.interfaces.fragments import UnifiedFragment_p
from acab.interfaces.value import Sentence_i
from acab.interfaces.value import ValueFactory as VF
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.modules.printing.default import DEFAULT_PRINTER
from acab.modules.semantics.default import DEFAULT_SEMANTICS

Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()

@dataclass
class ExampleExtension(UnifiedFragment_p):
    """
    A Minimal test extension
    Provides a simple dsl statement,
    and accompanying semantics and printer
    """
    signal      : str                     = field(default="ExampleExtension")
    constructor : None|Callable[..., Any] = field(default=None)
    printer     : None|Callable[..., Any] = field(default=None)
    semantic    : None|Callable[..., Any] = field(default=None)

    sem_val     : Any = field(default=None)

    def build_dsl(self) -> FI.DSL_Fragment_i:
        """
        return the DSL_Fragment describing this extension's parsing requirements and capabilities
        """
        constructor = lambda s,l,t: (VF.sen(data={"test_constructor": True, DS.SEMANTIC_HINT: self.signal}) << t[0])

        simple_dsl = pp.Literal("-[").suppress() + pp.Word(pp.alphas) + pp.Literal("]-").suppress()
        simple_dsl.set_name("test statement")
        simple_dsl.set_parse_action(constructor)

        dsl_fragment = FR.DSL_Fragment(specs=[],
                                       handlers=[ppDSL.PyParse_Handler("sentence.ends", func=simple_dsl)])

        return dsl_fragment

    def build_printers(self) -> FI.PrinterFragment_i:
        """
        return the PrinterFragment describing this extension's printing requirements and capabilities
        """
        printer = self.print_call

        print_fragment = FR.PrinterFragment(specs=[HandlerSpec(self.signal),
                                                   HandlerSpec(DSig.SENTENCE)],
                                         handlers=[Handler(signal=self.signal, func=printer)])


        return print_fragment

    def build_semantics(self) -> FI.Semantic_Fragment_i:
        """
        return the Semantic_Fragment describing this extension's semantic handlers
        """
        sem_call = self.semantic or self.sem_call
        sem_frag = FR.Semantic_Fragment(specs=[HandlerSpec(self.signal)],
                                        handlers=[Handler(signal=self.signal, func=sem_call)])
        return sem_frag

    def print_call(self, to_print:Value_A, *, top:None|PrintSystem_i=None, data:None|dict[str,Any]=None) -> list[str | Value_A]:
        assert(isinstance(to_print, Sentence_i))

        return ["-[ ", top.override(DSig.SENTENCE, to_print), " ]-"]

    def sem_call(self, instruction:Instruction, semSys:SemanticSystem, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet:
        self.sem_val = str(instruction)



class TestExampleExtension(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

    def setUp(self):
        # Set up the parser to ease test setup
        self.dsl   = ppDSL.PyParseDSL()
        self.dsl.register(EXLO_Parser)
        self.dsl.register(Component_DSL)
        self.dsl.build()


    def test_initial(self):
        instance = ExampleExtension()
        self.assertIsInstance(instance, ExampleExtension)
        self.assertTrue(hasattr(instance, "signal"))
        self.assertEqual(instance.signal, "ExampleExtension")

    def test_custom_signal(self):
        instance = ExampleExtension("test_signal")
        self.assertIsInstance(instance, ExampleExtension)
        self.assertTrue(hasattr(instance, "signal"))
        self.assertEqual(instance.signal, "test_signal")


    def test_dsl_build(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        self.assertIsInstance(dsl_frag, FI.DSL_Fragment_i)

    def test_printer_build(self):
        instance = ExampleExtension()
        print_frag = instance.build_printers()
        self.assertIsInstance(print_frag, FI.Printer_Fragment_i)

    def test_semantic_build(self):
        instance = ExampleExtension()
        sem_frag = instance.build_semantics()
        self.assertIsInstance(sem_frag, FI.Semantic_Fragment_i)


    def test_dsl(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        result   = dsl_frag.handlers[-1]("-[ test ]-")
        self.assertIsInstance(result[0], Sentence_i)
        self.assertEqual(result[0], "_:test")
        self.assertIn("test_constructor", result[0].data)

    def test_dsl_register(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        self.assertNotIn(dsl_frag.handlers[0], self.dsl.handler_specs['sentence.ends'].handlers)
        self.dsl.register(dsl_frag)
        self.dsl.build()
        self.assertIn(dsl_frag.handlers[0], self.dsl.handler_specs['sentence.ends'].handlers)
        result = self.dsl("a.b.c.-[ blah ]-")
        self.assertEqual(result[0], "_:a.b.c.[blah]")
        self.assertIn("test_constructor", result[0][-1].data)

    def test_printer1(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        print_frag = instance.build_printers()
        printer = DEFAULT_PRINTER()
        printer.register(print_frag)

        parsed = dsl_frag.handlers[-1]("-[ test ]-")[0]
        printed = printer(parsed)
        self.assertEqual(printed, "-[ test ]-")

    def test_printer2(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        print_frag = instance.build_printers()
        printer = DEFAULT_PRINTER()
        printer.register(print_frag)

        parsed = dsl_frag.handlers[-1]("-[ blah ]-")[0]
        printed = printer(parsed)
        self.assertEqual(printed, "-[ blah ]-")

    def test_printer3(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        print_frag = instance.build_printers()
        printer = DEFAULT_PRINTER()
        printer.register(print_frag)

        parsed = self.dsl("a.b.c.-[ blah ]-")[0]
        printed = printer(parsed)
        self.assertEqual(printed, "a.b.c.-[ blah ]-")



    def test_semantics(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        sem_sys = DEFAULT_SEMANTICS()
        sem_frag = instance.build_semantics()
        sem_sys.register(sem_frag)
        instr = self.dsl("-[ test ]-")[0][-1]
        self.assertIsNone(instance.sem_val)
        sem_sys(instr)
        self.assertEqual(instance.sem_val, "[test]")

    def test_semantics2(self):
        instance = ExampleExtension()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        sem_sys = DEFAULT_SEMANTICS()
        sem_frag = instance.build_semantics()
        sem_sys.register(sem_frag)
        instr = self.dsl("-[ blah ]-")[0][-1]
        self.assertIsNone(instance.sem_val)
        sem_sys(instr)
        self.assertEqual(instance.sem_val, "[blah]")



if __name__ == '__main__':
    unittest.main()
