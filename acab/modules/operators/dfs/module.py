#!/usr/bin/env python3

from acab import GET
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.interfaces.semantic import Semantic_Fragment
from acab.interfaces.handler_system import HandlerSpec
from acab.modules.semantics.statements import QueryPlusAbstraction
from acab.core.data.value import Sentence

from . import parser as DOP
from .semantics import DFSSemantics
from .printer import DFSSenPrinter

config = GET()

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

WALK_SEM_HINT    = Sentence.build([config.prepare("Module.DFSWalk", "WALK_SEM_HINT")()])

DFS_Sem_Frag = Semantic_Fragment(specs=[HandlerSpec(WALK_SEM_HINT)],
                                 handlers=[DFSSemantics().as_handler(),
                                           QueryPlusAbstraction().as_handler(signal="QUERY", flags=[DSL_Spec.flag_e.OVERRIDE])])


DFSQueryDSL = DSL_Fragment(specs=[DSL_Spec("word.constrained", struct=DOP.HOTLOAD_VAR),
                                  DSL_Spec("sentence.operator", struct=DOP.HOTLOAD_SEN_OP)],
                           handlers=[DSL_Handler("query.statement", DOP.dfs_query),
                                     DSL_Handler("action.statement", DOP.dfs_action)])

# TODO printer fragment
