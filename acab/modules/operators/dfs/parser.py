#!/usr/bin/env python3
from __future__ import annotations

from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.defaults.value_keys as DS
import pyparsing as pp
from acab import AcabConfig
from acab.core.parsing.consts import QUERY, op, orm, s_lit
from acab.core.util.sentences import ProductionComponent
from acab.core.value.sentence import Sentence
from acab.interfaces.value import ValueFactory as VF

config        = AcabConfig()

QUERY_HINT    = DS.QUERY
SEM_HINT      = DS.SEMANTIC_HINT
WALK_SEM_HINT = Sentence() << config.attr.Type.Primitive.INSTRUCT << config.attr.Semantic.Signals.WALK

def build_dfs_query(s, l, toks):
    words = []
    if 'root' in toks:
        assert(toks['root'].is_at_var)
        words.append(toks['root'])

    rest = toks['constraints'][:]
    assert(all([x.is_var for x in rest]))
    words += rest

    instruction = VF.sen(data={SEM_HINT: WALK_SEM_HINT, QUERY_HINT: True}) << words

    return instruction

def build_dfs_action(s, l, toks):
    words = []
    if 'root' in toks:
        assert(toks['root'].is_at_var)
        words.append(toks['root'])

    assert(isinstance(toks['action'], Sentence))
    words.append(toks['action'])

    # TODO refactor to be a ProductionComponent
    instruction = VF.sen(data={SEM_HINT: WALK_SEM_HINT}) << words

    return instruction


def build_dfs_operator(s, l, toks):
    words = []
    if 'root' in toks:
        assert(toks.root.is_at_var)
        words.append(toks.root)

    assert("op" in toks)
    words.append(toks.op)

    assert('vars' in toks)
    words += toks.vars[:]

    # TODO refactor to be a ProductionComponent
    instruction = VF.sen(data={SEM_HINT: WALK_SEM_HINT}) << words

    return instruction

# Parser: #####################################################################
HOTLOAD_VAR    = pp.Forward()
HOTLOAD_SEN_OP = pp.Forward()

dfs_operator  = pp.Literal(config.attr.Operator.Sugar.DFS).suppress()
dfs_head      = op(HOTLOAD_VAR("root")) + dfs_operator


dfs_query     = dfs_head + orm(HOTLOAD_VAR)("constraints") + QUERY
# TODO a dfs query with a subsentence query
# @a ᛦ $x.d.f?

# will build a sentence with a dfs semantic hint
dfs_query.set_parse_action(build_dfs_query)

# Build a dfs action sentence:
# ᛦ λarity.one.action.or.rule
# which will be applied to every node:
# λarity.one.action.or.rule $x
dfs_action    = dfs_head + HOTLOAD_SEN_OP('action')
dfs_action.set_parse_action(build_dfs_action)


HOTLOAD_TRANS_OP         = pp.Forward()
HOTLOAD_TRANS_OP.set_name("hl_trans_op")

dfs_operator = dfs_head + HOTLOAD_TRANS_OP("op") + orm(HOTLOAD_VAR)("vars")
dfs_operator.set_parse_action(build_dfs_operator)
