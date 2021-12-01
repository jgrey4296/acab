from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.interfaces.semantic as SI
import acab.modules.semantics.statements as ASem
from acab.core.config.config import GET
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.values import Sentence
from acab.interfaces.handler_system import Handler
from acab.modules.context import context_delayed_actions
from acab.modules.context.context_set import ContextSet
from acab.modules.semantics.basic_system import BasicSemanticSystem
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.structures.trie.default import DEFAULT_TRIE, DEFAULT_TRIE_SPEC
from acab.modules.structures.trie.trie_semantics import BreadthTrieSemantics

config = GET()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])

query_spec  = BasicSemanticSystem.Spec(QUERY_SEM_HINT).spec_from(SI.StatementSemantics_i)
action_spec = BasicSemanticSystem.Spec(ACTION_SEM_HINT).spec_from(SI.StatementSemantics_i)
rule_spec   = BasicSemanticSystem.Spec(RULE_SEM_HINT).spec_from(SI.StatementSemantics_i)
trans_spec  = BasicSemanticSystem.Spec(TRANSFORM_SEM_HINT).spec_from(SI.StatementSemantics_i)
cont_spec   = BasicSemanticSystem.Spec("_:CONTAINER").spec_from(SI.StatementSemantics_i)

def DEFAULT_HANDLER_SPECS():
    statements = [# Then abstractions / statements
        query_spec,
        trans_spec,
        action_spec,
        rule_spec,
        cont_spec]

    node_spec, trie_spec = DEFAULT_TRIE_SPEC()
    return statements + [node_spec, trie_spec]

def DEFAULT_HANDLERS():
    node_sem, trie_sem, trie_struct = DEFAULT_TRIE()
    trie_handler = trie_sem.as_handler(struct=trie_struct)

    query_sem   = ASem.QueryAbstraction().as_handler(QUERY_SEM_HINT)
    action_sem  = ASem.ActionAbstraction().as_handler(ACTION_SEM_HINT)
    rule_sem    = ASem.AtomicRuleAbstraction().as_handler(RULE_SEM_HINT)
    trans_sem   = ASem.TransformAbstraction().as_handler(TRANSFORM_SEM_HINT)
    cont_sem    = ASem.ContainerAbstraction().as_handler("_:CONTAINER")

    return [cont_sem, query_sem, action_sem, rule_sem,
            trans_sem, node_sem, trie_handler, trie_handler.as_handler("_:_default")]

def default_handlers_from_specs():
    node_spec, trie_spec = DEFAULT_TRIE_SPEC()
    node_sem, trie_sem, trie_struct  = DEFAULT_TRIE()

    trie_handler = trie_spec.on(trie_sem, struct=trie_struct),

    return [
        node_sem,
        query_spec.on(ASem.QueryAbstraction()),
        action_spec.on(ASem.ActionAbstraction()),
        rule_spec.on(ASem.AtomicRuleAbstraction()),
        trans_spec.on(ASem.TransformAbstraction()),
        cont_spec.on(ASem.ContainerAbstraction()),
        trie_handler,
        trie_handler.as_handler("_:_default")
        ]

# Build the default semantics
def DEFAULT_SEMANTICS():
    handlers = DEFAULT_HANDLERS()

    return BasicSemanticSystem(init_specs=handlers,
                               init_handlers=DEFAULT_HANDLERS())


def EXLO_SEMANTICS():
    node_sem    = ExclusionNodeSemantics().as_handler("_:atom")
    trie_sem    = BreadthTrieSemantics(init_handlers=[node_sem.as_handler("_:_default")])

    trie_handler = trie_sem.as_handler("_:trie",
                                       struct=BasicNodeStruct.build_default())

    handlers = DEFAULT_HANDLERS()
    handlers += [node_sem, trie_handler, trie_handler.as_handler("_:_default")]

    return BasicSemanticSystem(init_specs=DEFAULT_SPECS(),
                               init_handlers=handlers)

def EXLO_PROXY_SEMANTICS():
    exlo = EXLO_SEMANTICS()
    rule_sem  = ASem.ProxyRuleAbstraction().as_handler(RULE_SEM_HINT)
    return exlo.register_handler(rule_sem)
