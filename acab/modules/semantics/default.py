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
    node_sem    = BasicNodeSemantics().as_handler("_:node")
    trie_sem    = BreadthTrieSemantics(default_sem).as_handler("_:trie",
                                                               struct=BasicNodeStruct.build_default())

    query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
    action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
    rule_sem    = ASem.AtomicRuleAbstraction().as_handler(RULE_SEM_HINT)
    trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
    cont_sem    = ASem.ContainerAbstraction().as_handler("_:CONTAINER")

    return BasicSemanticSystem(in_handlers=[cont_sem,
                                            query_sem,
                                            action_sem,
                                            rule_sem,
                                            trans_sem,
                                            trie_sem],
                               default=trie_sem)

def EXLO_SEMANTICS():
    node_sem    = ExclusionNodeSemantics().as_handler("_:node")
    trie_sem    = BreadthTrieSemantics(default=node_sem).as_handler("_:trie",
                                                                    struct=BasicNodeStruct.build_default())

    query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
    action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
    rule_sem    = ASem.AtomicRuleAbstraction().as_handler(RULE_SEM_HINT)
    trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
    cont_sem    = ASem.ContainerAbstraction().as_handler("_:CONTAINER")

    return BasicSemanticSystem(handlers=[cont_sem,
                                         query_sem,
                                         action_sem,
                                         rule_sem,
                                         trans_sem,
                                         trie_sem],
                               default=trie_sem)

def EXLO_PROXY_SEMANTICS():
    node_sem    = ExclusionNodeSemantics().as_handler("_:node")
    trie_sem    = BreadthTrieSemantics(default=node_sem).as_handler("_:trie",
                                                                    struct=BasicNodeStruct.build_default())

    query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
    action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
    trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
    cont_sem    = ASem.ContainerAbstraction().as_handler("_:CONTAINER")

    rule_sem    = ASem.ProxyRuleAbstraction().as_handler(RULE_SEM_HINT)

    return BasicSemanticSystem(handlers=[cont_sem,
                                         query_sem,
                                         action_sem,
                                         rule_sem,
                                         trans_sem,
                                         trie_sem],
                               default=trie_sem)
