#!/usr/bin/env python3

from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.semantics.statements as ASem
from acab import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.value.sentence import Sentence
from acab.interfaces.handler_system import Handler
from acab.modules.context import context_delayed_actions
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.structures.trie.semantics import BreadthTrieSemantics
from acab.modules.semantics.values import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)
config = AcabConfig()
DEFAULT_HANDLER_SIGNAL = config.attr.Handler.System.DEFAULT_SIGNAL

QUERY_SEM_HINT     = Sentence() << config.attr.Semantic.Signals.QUERY
ACTION_SEM_HINT    = Sentence() << config.attr.Semantic.Signals.ACTION
TRANSFORM_SEM_HINT = Sentence() << config.attr.Semantic.Signals.TRANSFORM
RULE_SEM_HINT      = Sentence() << config.attr.Semantic.Signals.RULE
AGENDA_SEM_HINT    = Sentence() << config.attr.Semantic.Signals.AGENDA
LAYER_SEM_HINT     = Sentence() << config.attr.Semantic.Signals.LAYER
PIPELINE_SEM_HINT  = Sentence() << config.attr.Semantic.Signals.PIPELINE

def DEFAULT_TRIE(name="trie"):
    node_sem    = BasicNodeSemantics("node")
    trie_sem    = BreadthTrieSemantics(name, init_handlers=[node_sem.as_handler(DEFAULT_HANDLER_SIGNAL)])

    trie_struct = BasicNodeStruct.build_default(name)
    return (trie_sem, trie_struct)
