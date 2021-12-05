#!/usr/bin/env python3

from acab import GET
from acab.interfaces.dsl import DSL_Fragment_i
from acab.interfaces.semantic import Semantic_Fragment
from acab.interfaces.handler_system import HandlerSpec
from acab.modules.semantics.statements import QueryPlusAbstraction

from . import dfs_op_parser as DOP
from .walk_semantics import WalkTrieSemantics
from .dfs_printer import DFSSenPrinter

config = GET()

WALK_SEM_HINT    = config.prepare("Module.DFSWalk", "WALK_SEM_HINT")()

# TODO dfs spec
DFS_Sem_Frag = Semantic_Fragment(specs=[HandlerSpec(WALK_SEM_HINT)],
                                 statement=[WalkTrieSemantics().as_handler(),
                                            QueryPlusAbstraction().as_handler("_:QUERY")])


class DFSQueryDSL(DSL_Fragment_i):
    """ The Module Spec for base operators """

    def assert_parsers(self, pt):
        pt.add("query.statement.dfs", DOP.dfs_query)

    def query_parsers(self, pt):
        DOP.HOTLOAD_VAR << pt.query("word.constrained")
        DOP.HOTLOAD_SEN_OP << pt.query("sentence.operator")
