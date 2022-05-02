#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import pyparsing as pp

from acab import AcabConfig
from  acab.core.parsing.consts import orm, QUERY, op, s_lit
from acab.core.value.sentence import Sentence
from acab.core.value.instruction import ProductionComponent

config        = AcabConfig()

QUERY_HINT    = config.prepare("Value.Structure", "QUERY")()
SEM_HINT      = config.prepare("Value.Structure", "SEMANTIC_HINT")()
WALK_SEM_HINT = Sentence([config.prepare("Semantic.Signals", "WALK")()])

def build_dfs_query(s, l, toks):
    words = []
    if 'root' in toks:
        assert(toks['root'].is_at_var)
        words.append(toks['root'])

    rest = toks['constraints'][:]
    assert(all([x.is_var for x in rest]))
    words += rest

    instruction = Sentence(words, data={SEM_HINT: WALK_SEM_HINT, QUERY_HINT: True})

    return instruction

def build_dfs_action(s, l, toks):
    words = []
    if 'root' in toks:
        assert(toks['root'].is_at_var)
        words.append(toks['root'])

    assert(isinstance(toks['action'], Sentence))
    words.append(toks['action'])

    # TODO refactor to be a ProductionComponent
    instruction = Sentence(words, data={SEM_HINT: WALK_SEM_HINT})

    return instruction


# Parser: #####################################################################
HOTLOAD_VAR    = pp.Forward()
HOTLOAD_SEN_OP = pp.Forward()

dfs_operator  = pp.Literal("ᛦ").suppress()
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
