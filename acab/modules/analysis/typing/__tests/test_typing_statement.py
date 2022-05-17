import logging as logmod
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
import unittest
from unittest import mock
import pyparsing as pp
import warnings

import acab
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()
    if '@pytest_ar' in globals():
        from acab.core.parsing import debug_funcs as DBF
        DBF.debug_pyparsing(pp.Diagnostics.enable_debug_on_named_expressions)


import acab.interfaces.value as VI
from acab.core.defaults import value_keys as DS
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.modules.parsing.exlo.exlo_dsl import EXLO_Parser
from acab.core.parsing.component_dsl import Component_DSL
from acab.interfaces import fragments as FI
from acab.modules.printing.default import DEFAULT_PRINTER

from acab.modules.analysis.typing.module import TypingExtension

class TestTypingStatement(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.addHandler(file_h)
        logging.root.setLevel(logmod.NOTSET)

        # Set up the parser to ease test setup
        cls.dsl   = ppDSL.PyParseDSL()
        cls.dsl.register(EXLO_Parser)
        cls.dsl.register(Component_DSL)

        cls.dsl.build()


    def test_initial(self):
        instance = TypingExtension()
        self.assertIsInstance(instance, TypingExtension)
        self.assertTrue(hasattr(instance, "signal"))

    def test_dsl_build(self):
        instance = TypingExtension()
        dsl_frag = instance.build_dsl()
        self.assertIsInstance(dsl_frag, FI.DSL_Fragment_i)

    def test_printer_build(self):
        instance = TypingExtension()
        print_frag = instance.build_printers()
        self.assertIsInstance(print_frag, FI.Printer_Fragment_i)

    def test_semantic_build(self):
        instance = TypingExtension()
        sem_frag = instance.build_semantics()
        self.assertIsInstance(sem_frag, FI.Semantic_Fragment_i)

    def test_dsl(self):
        instance = TypingExtension()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        result = dsl_frag.handlers[0]("⊢ a.b.c ∈ d.e.f")[0]
        self.assertEqual(result.data[DS.SEMANTIC_HINT], "TYPE_CHECK")
        self.assertIn("loc", result)
        self.assertIn("def", result)
        self.assertIsInstance(result['loc'], VI.Sentence_i)
        self.assertIsInstance(result['def'], VI.Sentence_i)

    def test_print(self):
        print_sys = DEFAULT_PRINTER()
        instance = TypingExtension()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        instr = dsl_frag.handlers[0]("⊢ a.b.c ∈ d.e.f")[0]

        printer  = instance.build_printers()
        print_sys.register(printer)
        result = print_sys.pprint(instr)
        self.assertEqual(result, "⊢ a.b.c ∈ d.e.f")

    def test_print2(self):
        print_sys = DEFAULT_PRINTER()
        instance = TypingExtension()
        dsl_frag = instance.build_dsl()
        self.dsl.register(dsl_frag)
        self.dsl.build()
        instr = dsl_frag.handlers[0]("⊢ a.different!sen ∈ blah!bloo")[0]

        printer  = instance.build_printers()
        print_sys.register(printer)
        result = print_sys.pprint(instr)
        self.assertEqual(result, "⊢ a.different!sen ∈ blah!bloo")

if __name__ == '__main__':
    unittest.main()
