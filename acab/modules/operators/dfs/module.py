#!/usr/bin/env python3

from acab import GET
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.semantics.basic import Semantic_Fragment
from acab.core.util.part_implementations.handler_system import HandlerSpec
from acab.modules.semantics.statements import QueryPlusAbstraction, ActionPlusAbstraction
from acab.core.value.sentence import Sentence

from . import parser as DOP
from .semantics import DFSSemantics
from .printer import DFSSenPrinter

config = GET()

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

WALK_SEM_HINT   = Sentence([config.prepare("Semantic.Signals", "WALK")()])
QUERY_SEM_HINT  = Sentence([config.prepare("Semantic.Signals", "QUERY")()])
ACTION_SEM_HINT = Sentence([config.prepare("Semantic.Signals", "ACTION")()])

DFS_Sem_Frag = Semantic_Fragment(specs=[HandlerSpec(WALK_SEM_HINT)],
                                 handlers=[
                                     DFSSemantics().as_handler(),
                                     QueryPlusAbstraction().as_handler(signal=QUERY_SEM_HINT, flags=[DSL_Spec.flag_e.OVERRIDE]),
                                     ActionPlusAbstraction().as_handler(signal=ACTION_SEM_HINT, flags=[DSL_Spec.flag_e.OVERRIDE])
                                 ])


DFS_DSL = DSL_Fragment(specs=[DSL_Spec("word.constrained", struct=DOP.HOTLOAD_VAR),
                              DSL_Spec("sentence.operator", struct=DOP.HOTLOAD_SEN_OP)],
                       handlers=[DSL_Handler("query.statement", DOP.dfs_query),
                                 DSL_Handler("action.statement", DOP.dfs_action)])

# TODO printer fragment
