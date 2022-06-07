#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

import pyparsing as pp

logging = root_logger.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass


from acab import AcabConfig

config = AcabConfig()

from acab.interfaces import value as VI
import acab.core.defaults.value_keys as DS
from acab.core.defaults.semantic_signals import signals as DSig
import acab.modules.analysis.typing.printer as TPR
from acab.core.config.config import AcabConfig
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.semantics.basic import StatementSemantics
from acab.core.util.fragments import (DSL_Fragment, PrinterFragment,
                                      Semantic_Fragment)
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.interfaces import fragments as FI
from acab.interfaces import data as DI
from acab.interfaces import handler_system as HS
from acab.interfaces.bind import Bind_i
from acab.interfaces.fragments import UnifiedFragment_p
from acab.interfaces.semantic import StatementSemantics_i
from acab.interfaces.value import ValueFactory as VF

from .parsing import TypeDefParser as TDefP
from .parsing import TypeParser as TP
from .unify import type_unify_fns as tuf
from .unify.util import gen_f

import acab.modules.values.binding.variable_control as VC

config = AcabConfig()
Handler = config.prepare("Imports.Targeted", "handler", actions=[config.actions_e.IMCLASS], args={"interface": HS.Handler_i})()
Bind    = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

TYPE_INSTANCE = config.attr.Print.Signals.TYPE_INSTANCE
TYPE_DEF      = config.attr.Print.Signals.TYPE_DEF
SUM_TYPE      = config.attr.Print.Signals.SUM_TYPE
OP_DEF        = config.attr.Print.Signals.OP_DEF
TYPE_CLASS    = config.attr.Print.Signals.TYPE_CLASS

LinkSignalTo = lambda x, y: DSL_Spec(x, struct=y, flags=[DSL_Spec.flag_e.COLLECT])

@dataclass
class CheckStatementFragment(UnifiedFragment_p):
    """
    Prototype TypeCheckSemantics which provides the parser, printer,
    semantic call, and means to hook into acab
    """
    signal      : str                     = field(default="TYPE_CHECK")
    constructor : None|Callable[..., Any] = field(default=None)
    printer     : None|Callable[..., Any] = field(default=None)
    semantic    : None|Callable[..., Any] = field(default=None)

    def build_dsl(self) -> FI.DSL_Fragment_i:
        """
        return the DSL_Fragment describing this extension's parsing requirements and capabilities
        """
        constructor = self.constructor or self._build_instruction

        HOTLOAD_SEN = pp.Forward()

        TypingStatement= (pp.Keyword("⊢").suppress() + HOTLOAD_SEN("loc")
                          + pp.Keyword("∈").suppress() + HOTLOAD_SEN("def"))
        TypingStatement.set_name("TypingStatment")
        TypingStatement.set_parse_action(constructor)
        TypingStatement.streamline()


        dsl_fragment = DSL_Fragment(specs=[LinkSignalTo("sentence", HOTLOAD_SEN)],
                                    handlers=[ppDSL.PyParse_Handler("transform.statement", func=TypingStatement)])

        return dsl_fragment

    def build_printers(self) -> FI.Printer_Fragment_i:
        """
        return the PrinterFragment describing this extension's printing requirements and capabilities
        """
        printer = self.printer or self.basic_printer

        print_fragment = PrinterFragment(specs=[HandlerSpec(self.signal)],
                                         handlers=[Handler(signal=self.signal, func=printer)])


        return print_fragment

    def build_semantics(self) -> FI.Semantic_Fragment_i:
        """
        return the Semantic_Fragment describing this extension's semantic handlers
        """
        sem_call = self.semantic or self.sem_call
        sem_frag = Semantic_Fragment(specs=[HandlerSpec(self.signal)],
                                     handlers=[Handler(signal=self.signal, func=sem_call)])
        return sem_frag


    def _build_instruction(self, toks) -> list[Sentence]:
        """
        The default constructor for the extension's parser -> internal acab data
        """
        instr = VF.sen(data={DS.SEMANTIC_HINT: self.signal}) << toks['loc'].copy(name="loc") << toks['def'].copy(name="def")
        return [instr]

    def basic_printer(self, value, top=None, data=None):
        """
        The default printing routine for the extension's internal acab data -> str
        """
        ret_list = ["⊢ ", value['loc'], " ∈ ", value['def']]
        return ret_list



    def sem_call(self, instruction:Instruction, semSys:SemanticSystem, *, ctxs:None|CtxSet=None, data:None|dict[str,Any]=None) -> CtxSet:
        """
        The default semantic implementation for the extension

         ⊢ [loc] : [def]
        """
        loc       : DI.StructView  = Bind.bind(instruction['loc'], ctxs[0])
        the_type  : TypeDefinition = Bind.bind(instruction['def'], ctxs[0])
        subctx    = {}

        # sem = semSys.lookup(loc)
        to_check  : list[Sentence] = loc.sem.to_sentences(loc)
        type_sens : list[Sentence] = [VC.rectx(x, ctx=subctx) for x in the_type[0].to_sentences()]
        # TODO get to_check down to depth of max(type_sens)

        new_var  = gen_f()
        type_sens = [new_var.add(x) for x in type_sens]

        # unify them:
        unified  = tuf.type_unify.repeat(to_check, type_sens, ctxs[0])
        result   = [tuf.type_unify.apply(x, unified) for x in type_sens]

        return result
