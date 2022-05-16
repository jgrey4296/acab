#!/usr/bin/env python3

from acab import AcabConfig
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.util.fragments import Semantic_Fragment, DSL_Fragment
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.modules.semantics.statements import QueryPlusAbstraction, ActionPlusAbstraction
from acab.core.value.sentence import Sentence

from . import parser as DOP
from .semantics import DFSSemantics
from .printer import DFSSenPrinter

config = AcabConfig()

DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

WALK_SIGNAL   = Sentence() << config.attr.Semantic.Signals.WALK
QUERY_SIGNAL  = Sentence() << config.attr.Semantic.Signals.QUERY
ACTION_SIGNAL = Sentence() << config.attr.Semantic.Signals.ACTION

DFS_Sem_Frag = Semantic_Fragment(specs=[HandlerSpec(WALK_SIGNAL)],
                                 handlers=[
                                     DFSSemantics().as_handler(),
                                     QueryPlusAbstraction().as_handler(signal=QUERY_SIGNAL, flags=[DSL_Spec.flag_e.OVERRIDE]),
                                     ActionPlusAbstraction().as_handler(signal=ACTION_SIGNAL, flags=[DSL_Spec.flag_e.OVERRIDE])
                                 ])


DFS_DSL = DSL_Fragment(specs=[DSL_Spec("word.constrained", struct=DOP.HOTLOAD_VAR),
                              DSL_Spec("sentence.operator", struct=DOP.HOTLOAD_SEN_OP)],
                       handlers=[DSL_Handler("query.statement", func=DOP.dfs_query),
                                 DSL_Handler("action.statement", func=DOP.dfs_action)])

# TODO printer fragment
