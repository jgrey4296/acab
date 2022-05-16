from __future__ import annotations
import logging as logmod
import unittest
import warnings
from os.path import split, splitext
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, TypeAlias,
                    TypeVar, cast)
from dataclasses import dataclass, field, InitVar

import pyparsing as pp
import acab

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

Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()

__all__ = ["AltExampleExtension"]

@dataclass
class AltExampleExtension(FI.UnifiedFragment_p):
    """
    A Minimal test extension
    Provides a simple dsl statement,
    and accompanying semantics and printer
    """
    signal      : str                     = field(default="AltExampleExtension")
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

