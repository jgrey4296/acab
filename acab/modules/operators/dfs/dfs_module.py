#!/usr/bin/env python3

from acab.interfaces.dsl import DSL_Fragment_i
from acab.interfaces.semantic import Semantic_Fragment

from . import dfs_op_parser as DOP
from .walk_semantics import WalkTrieSemantics

DFS_Sem_Frag = Semantic_Fragment(abstraction=[WalkTrieSemantics().as_handler()])


class DFSQueryDSL(DSL_Fragment_i):
    """ The Module Spec for base operators """

    def assert_parsers(self, pt):
        pt.add("query.statement.dfs", DOP.dfs_query)

    def query_parsers(self, pt):
        DOP.HOTLOAD_VAR << pt.query("word.constrained")
