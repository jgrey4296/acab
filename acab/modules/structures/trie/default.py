#!/usr/bin/env python3
"""
The standard setup of Trie Semantics

"""
##-- imports
from __future__ import annotations
import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.semantics.statements as ASem
import acab
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.util.part_implementations.handler_system import Handler
from acab.core.value.sentence import Sentence
from acab.interfaces import semantic as SI
from acab.modules.context import context_delayed_actions
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.structures.trie.semantics import FlattenBreadthTrieSemantics

##-- end imports

logging = logmod.getLogger(__name__)
config  = acab.config

DEFAULT_HANDLER_SIGNAL = config.handler.system.DEFAULT_SIGNAL
ATOM_SIGNAL            = Sentence() << config.data.TYPE_BASE

TRIE_SIGNAL              = Sentence() << config.semantic.signals.TRIE

def DEFAULT_TRIE_SPEC(name=TRIE_SIGNAL):
    logging.debug("Constructing Default Trie Semantics Spec")
    node_spec   = BasicSemanticSystem.Spec(ATOM_SIGNAL).spec_from(SI.ValueSemantics_i)
    trie_spec   = BasicSemanticSystem.Spec(name).spec_from(SI.StructureSemantics_i)

    return node_spec, trie_spec

def DEFAULT_TRIE(name=TRIE_SIGNAL):
    logging.debug("Constructing Default Trie Structure, Semantics and Handlers")
    node_handler = BasicNodeSemantics(ATOM_SIGNAL).as_handler()
    trie_sem     = FlattenBreadthTrieSemantics(signal=name,
                                               init_specs=DEFAULT_TRIE_SPEC(),
                                               init_handlers=[node_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])

    # Attach a default struct to the semantics
    trie_handler = trie_sem.as_handler(struct=BasicNodeStruct.build_default())

    return (node_handler, trie_handler)
