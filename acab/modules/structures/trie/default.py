#!/usr/bin/env python3

from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.semantics.statements as ASem
from acab.core.config.config import GET
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.value import Sentence
from acab.interfaces.handler_system import Handler
from acab.modules.context import context_delayed_actions
from acab.modules.context.context_set import ContextSet
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.structures.trie.trie_semantics import BreadthTrieSemantics
from acab.modules.semantics.values import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)
from acab.interfaces import semantic as SI

config = GET()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])

def DEFAULT_TRIE_SPEC(name="_:trie"):
    node_spec   = BasicSemanticSystem.Spec("_:atom").spec_from(SI.ValueSemantics_i)
    trie_spec   = BasicSemanticSystem.Spec(name).spec_from(SI.StructureSemantics_i)

    return node_spec, trie_spec

def DEFAULT_TRIE(name="_:trie"):
    node_handler = BasicNodeSemantics("_:atom").as_handler()
    trie_sem     = BreadthTrieSemantics(signal=name,
                                        init_handlers=[node_handler.as_handler("_:_default")])

    trie_handler = trie_sem.as_handler(struct=BasicNodeStruct.build_default())

    return (node_handler, trie_handler)
