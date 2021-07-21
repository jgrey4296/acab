from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.semantics.abstractions as ASem
from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.modules.semantics.dependent import BreadthTrieSemantics
from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)
from acab.modules.semantics.system import BasicSemanticSystem
from acab.abstract.core.values import Sentence
from acab.abstract.config.config import GET

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
    node_sem    = BasicNodeSemantics("_:node")
    trie_sem    = BreadthTrieSemantics("_:trie", default=(node_sem, None),
                                       handlers=[], structs=[])

    query_sem   = ASem.QueryAbstraction(QUERY_SEM_HINT)
    action_sem  = ASem.ActionAbstraction(ACTION_SEM_HINT)
    rule_sem    = ASem.AtomicRuleAbstraction(RULE_SEM_HINT)
    trans_sem   = ASem.TransformAbstraction(TRANSFORM_SEM_HINT)
    cont_sem    = ASem.ContainerAbstraction("_:CONTAINER")

    trie_struct = BasicNodeStruct.build_default("_:trie")


    return BasicSemanticSystem(handlers=[cont_sem,
                                         query_sem,
                                         action_sem,
                                         rule_sem,
                                         trans_sem,
                                         trie_sem],
                               structs=[trie_struct],
                               default=(trie_sem, trie_struct))
