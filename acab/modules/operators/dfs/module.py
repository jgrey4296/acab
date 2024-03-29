#!/usr/bin/env python3
##-- imports
from __future__ import annotations

from dataclasses import InitVar, dataclass, field

import acab.core.defaults.value_keys as DS
import pyparsing as pp
import acab
from acab.core.defaults import print_signals as DSig
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.parsing.consts import QUERY, op, orm, s_lit
from acab.core.util import fragments as FR
from acab.core.util.fragments import DSL_Fragment, Semantic_Fragment
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.core.util.sentences import ProductionComponent
from acab.core.value.sentence import Sentence
from acab.interfaces import fragments as FI
from acab.interfaces import handler_system as HS
from acab.interfaces.fragments import UnifiedFragment_p
from acab.interfaces.value import Sentence_i
from acab.interfaces.value import ValueFactory as VF
from acab.modules.semantics.statements import (ActionPlusAbstraction,
                                               QueryPlusAbstraction)

from . import parser as DOP
from .semantics import DFSSemantics

##-- end imports

config = acab.config

# TODO import
Handler = config.imports.specific.handler

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

INSTRUCT_SIGNAL = VF.sen() << config.types.primitive.INSTRUCT
WALK_SIGNAL     = INSTRUCT_SIGNAL << config.semantic.signals.WALK
CONTAIN_SIGNAL  = INSTRUCT_SIGNAL << config.types.primitive.CONTAINER
QUERY_SIGNAL    = CONTAIN_SIGNAL  << config.types.primitive.QUERY
ACTION_SIGNAL   = CONTAIN_SIGNAL  << config.types.primitive.ACTION

# TODO printer fragment
@dataclass
class DFSExtension(UnifiedFragment_p):
    """
    A Minimal test extension
    Provides a simple dsl statement,
    and accompanying semantics and printer
    """
    signal      : str = field(default=WALK_SIGNAL)

    def build_dsl(self) -> FI.DSL_Fragment_i:
        """
        return the DSL_Fragment describing this extension's parsing requirements and capabilities
        """
        dsl_fragment = DSL_Fragment(specs=[DSL_Spec("word.constrained", struct=DOP.HOTLOAD_VAR),
                                           DSL_Spec("operators.transform",  struct=DOP.HOTLOAD_TRANS_OP),
                                           DSL_Spec("sentence.operator", struct=DOP.HOTLOAD_SEN_OP)],
                                    handlers=[DSL_Handler("query.statement", func=DOP.dfs_query),
                                              DSL_Handler("action.statement", func=DOP.dfs_action),
                                              DSL_Handler("action.statement", func=DOP.dfs_operator)])
        return dsl_fragment

    def build_printer(self) -> FI.PrinterFragment_i:
        """
        return the PrinterFragment describing this extension's printing requirements and capabilities
        """
        printer = self._print_call

        print_fragment = FR.PrinterFragment(specs=[HandlerSpec(self.signal),
                                                   HandlerSpec(DSig.SENTENCE)],
                                         handlers=[Handler(signal=self.signal, func=printer)])


        return print_fragment

    def build_semantics(self) -> FI.Semantic_Fragment_i:
        """
        return the Semantic_Fragment describing this extension's semantic handlers
        """
        sem_frag = Semantic_Fragment(specs=[HandlerSpec(WALK_SIGNAL)],
                                         handlers=[
                                             DFSSemantics().as_handler(),
                                             QueryPlusAbstraction().as_handler(signal=QUERY_SIGNAL, flags=[DSL_Spec.flag_e.OVERRIDE]),
                                             ActionPlusAbstraction().as_handler(signal=ACTION_SIGNAL, flags=[DSL_Spec.flag_e.OVERRIDE])
                                         ])
        return sem_frag

    def _print_call(self, to_print:Value_A, *, top:None|PrintSystem_i=None, data:None|dict[str,Any]=None) -> list[str | Value_A]:
        assert(isinstance(value, VI.Sentence_i))
        return_list = []
        if value[0].is_at_var:
            return_list.append(top.override(False, value[0], data={"no_modal": True}))
            return_list.append(" ᛦ ")
            return_list += [top.override(False, x, data={"no_modal": True}) for x in value.words[1:]]
        else:
            return_list.append("ᛦ ")
            return_list += [top.override(False, x, data={"no_modal": True}) for x in value.words]


        return return_list

