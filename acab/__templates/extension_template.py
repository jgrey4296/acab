#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
import warnings
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)

import acab
import pyparsing as pp

config = acab.AcabConfig()

from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.printing import default_signals as DSig
from acab.core.util import fragments as FR
from acab.core.semantics.basic import StatementSemantics
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.core.value import default_structure as DS
from acab.interfaces import handler_system as HS
from acab.interfaces import fragments as FI
from acab.interfaces.semantic import StatementSemantics_i
from acab.interfaces.unified_extension import UnifiedExtension_p
from acab.interfaces.value import Sentence_i
from acab.interfaces.value import ValueFactory as VF

Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()

@dataclass
class ExampleExtension(UnifiedExtension_p):
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

    def build_printer(self) -> FI.PrinterFragment_i:
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
