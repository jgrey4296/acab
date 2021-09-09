from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.semantics.abstractions as ASem
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.abstract.core.values import Sentence
from acab.abstract.config.config import GET
from acab.abstract.interfaces.handler_system import Handler

config = GET()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])

# Build the default semantics
def DEFAULT_SEMANTICS():
    node_sem    = Handler("_:node", func=BaiscNodeSemantics)
    trie_sem    = Handler("_:trie",
                          func=BreadthTrieSemantics(in_handlers=[], default=node_sem),
                          struct=BasicNodeStruct)

    query_sem   = Handler(QUERY_SEM_HINT, ASem.QueryAbstraction)
    action_sem  = Handler(ACTION_SEM_HINT, ASem.ActionAbstraction)
    rule_sem    = Handler(RULE_SEM_HINT, ASem.AtomicRuleAbstraction)
    trans_sem   = Handler(TRANSFORM_SEM_HINT, ASem.TransformAbstraction)
    cont_sem    = Handler("_:CONTAINER", ASem.ContainerAbstraction)



    return BasicSemanticSystem(in_handlers=[cont_sem,
                                         query_sem,
                                         action_sem,
                                         rule_sem,
                                         trans_sem,
                                         trie_sem],
                               default=trie_sem)

def EXLO_SEMANTICS():
    node_sem    = Handler("_:node", func=ExclusionNodeSemantics)
    trie_sem    = Handler("_:trie",
                          func=BreadthTrieSemantics(in_handlers=[], default=node_sem),
                          struct=BasicNodeStruct)

    query_sem   = Handler(QUERY_SEM_HINT, ASem.QueryAbstraction)
    action_sem  = Handler(ACTION_SEM_HINT, ASem.ActionAbstraction)
    rule_sem    = Handler(RULE_SEM_HINT, ASem.AtomicRuleAbstraction)
    trans_sem   = Handler(TRANSFORM_SEM_HINT, ASem.TransformAbstraction)
    cont_sem    = Handler("_:CONTAINER", ASem.ContainerAbstraction)



    return BasicSemanticSystem(in_handlers=[cont_sem,
                                         query_sem,
                                         action_sem,
                                         rule_sem,
                                         trans_sem,
                                         trie_sem],
                               default=trie_sem)
