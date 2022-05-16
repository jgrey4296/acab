from __future__ import annotations

import logging as logmod
import unittest
import warnings
from dataclasses import InitVar, dataclass, field
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)

import acab
import pyparsing as pp

config = acab.AcabConfig()

import acab.core.defaults.value_keys as DS
from acab.core.defaults import print_signals as DSig
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.util import fragments as FR
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.interfaces import fragments as FI
from acab.interfaces import handler_system as HS
from acab.interfaces.value import Sentence_i
from acab.interfaces.value import ValueFactory as VF
from acab.modules.operators.query.query_operators import NEQ, EQ

Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()

def print_call(to_print:Value_A, *, top:None|PrintSystem_i=None, data:None|dict[str,Any]=None) -> list[str | Value_A]:
    assert(isinstance(to_print, Sentence_i))
    return ["-[ ", top.override(DSig.SENTENCE, to_print), " ]-"]

def sem_call(instruction:Instruction, semSys:SemanticSystem, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet:
    pass

constructor = lambda s,l,t: (VF.sen(data={"test_constructor": True, DS.SEMANTIC_HINT: "signal"}) << t[0])

simple_dsl = pp.Literal("-[").suppress() + pp.Word(pp.alphas) + pp.Literal("]-").suppress()
simple_dsl.set_name("test statement")
simple_dsl.set_parse_action(constructor)

dsl_fragment = FR.DSL_Fragment(specs=[],
                               handlers=[ppDSL.PyParse_Handler("sentence.ends", func=simple_dsl)])


print_fragment = FR.PrinterFragment(specs=[HandlerSpec("signal"),
                                           HandlerSpec(DSig.SENTENCE)],
                                    handlers=[Handler(signal="signal", func=print_call)])
