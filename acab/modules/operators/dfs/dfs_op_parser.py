#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import pyparsing as pp

from acab.core.config.config import GET
from  acab.core.parsing.consts import orm, QUERY, op, s_lit
from acab.core.data.values import Sentence
config        = GET()

QUERY         = s_lit(config.prepare("Symbols", "QUERY")())
QUERY_HINT    = config.prepare("Value.Structure", "QUERY")()
SEM_HINT      = config.prepare("Value.Structure", "SEMANTIC_HINT")()
WALK_SEM_HINT = Sentence.build([config.prepare("Module.DFSWalk", "WALK_SEM_HINT")()])

def build_dfs_query(s, l, toks):
    words = []
    if 'root' in toks:
        assert(toks['root'].is_at_var)
        words.append(toks['root'])

    rest = toks['constraints'][:]
    assert(all([x.is_var for x in rest]))
    words += rest

    query = Sentence.build(words,
                           data={SEM_HINT: WALK_SEM_HINT})
    query[-1].data[QUERY] = True

    return query

# Parser: #####################################################################
HOTLOAD_VAR   = pp.Forward()

dfs_operator  = pp.Literal("ᛦ").suppress()

dfs_query     = op(HOTLOAD_VAR("root")) + dfs_operator + orm(HOTLOAD_VAR)("constraints") + QUERY
# will build a sentence with a dfs semantic hint
dfs_query.setParseAction(build_dfs_query)

