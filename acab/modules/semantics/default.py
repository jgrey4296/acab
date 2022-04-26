from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.interfaces.semantic as SI
import acab.modules.semantics.statements as ASem
from acab.core.config.config import GET
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.value.sentence import Sentence
from acab.core.util.part_implementations.handler_system import Handler, HandlerSpec
from acab.modules.context import context_delayed_actions
from acab.modules.context.context_set import ContextSet
from acab.modules.semantics.basic_system import BasicSemanticSystem as BSS
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.structures.trie.default import DEFAULT_TRIE, DEFAULT_TRIE_SPEC
from acab.modules.structures.trie.semantics import FlattenBreadthTrieSemantics

config = GET()

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()
QUERY_SEM_HINT         = Sentence([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT        = Sentence([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT     = Sentence([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT          = Sentence([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT        = Sentence([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT         = Sentence([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT      = Sentence([config.prepare("SEMANTICS", "PIPELINE")()])
CONTAINER_SEM_HINT     = Sentence([config.prepare("SEMANTICS", "CONTAINER")()])
ATOM_HINT              = Sentence([config.prepare("SEMANTICS", "ATOM")()])

query_spec  = BSS.Spec(QUERY_SEM_HINT).spec_from(SI.StatementSemantics_i)
action_spec = BSS.Spec(ACTION_SEM_HINT).spec_from(SI.StatementSemantics_i)
rule_spec   = BSS.Spec(RULE_SEM_HINT).spec_from(SI.StatementSemantics_i)
trans_spec  = BSS.Spec(TRANSFORM_SEM_HINT).spec_from(SI.StatementSemantics_i)
cont_spec   = BSS.Spec(CONTAINER_SEM_HINT).spec_from(SI.StatementSemantics_i)

def DEFAULT_SPECS():
    """
    Return the basic specs a semantic system needs to respond to
    """
    statements = [# Then abstractions / statements
        query_spec,
        trans_spec,
        action_spec,
        rule_spec,
        cont_spec]

    node_spec, trie_spec = DEFAULT_TRIE_SPEC()
    return statements + [node_spec, trie_spec]

def DEFAULT_HANDLERS():
    """
    The Default handlers for the default specs
    Duplicates the trie handler, so it is both default
    and explicitly addressable
    """
    node_handler, trie_handler = DEFAULT_TRIE()

    query_handler   = ASem.QueryAbstraction().as_handler(signal=QUERY_SEM_HINT)
    action_handler  = ASem.ActionAbstraction().as_handler(signal=ACTION_SEM_HINT)
    rule_handler    = ASem.AtomicRuleAbstraction().as_handler(signal=RULE_SEM_HINT)
    trans_handler   = ASem.TransformAbstraction().as_handler(signal=TRANSFORM_SEM_HINT)
    cont_handler    = ASem.ContainerAbstraction().as_handler(signal=CONTAINER_SEM_HINT)

    return [cont_handler, query_handler, action_handler, rule_handler,
            trans_handler, node_handler, trie_handler, trie_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)]

def default_handlers_from_specs():
    """
    An alternative way to build the default handlers,
    using the specs themselves
    """
    node_spec, trie_spec       = DEFAULT_TRIE_SPEC()
    node_handler, trie_handler = DEFAULT_TRIE()

    return [
        query_spec.on(ASem.QueryAbstraction()),
        action_spec.on(ASem.ActionAbstraction()),
        rule_spec.on(ASem.AtomicRuleAbstraction()),
        trans_spec.on(ASem.TransformAbstraction()),
        cont_spec.on(ASem.ContainerAbstraction()),
        node_handler,
        trie_handler,
        trie_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)
        ]

# Build the default semantics
def DEFAULT_SEMANTICS():
    """
    Create a BasicSemanticSystem with default signal specs,
    and default handlers for those specs
    """
    return BSS(init_specs=DEFAULT_SPECS(),
               init_handlers=DEFAULT_HANDLERS())


def EXLO_SEMANTICS():
    """
    Instead of simple node semantics, builds a basic semantic system
    with exclusion logic semantics for nodes
    """
    node_handler = ExclusionNodeSemantics().as_handler(signal=ATOM_HINT)
    trie_sem     = FlattenBreadthTrieSemantics(init_specs=[],
                                               sieve_fns=[],
                                               init_handlers=[node_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])


    trie_handler = trie_sem.as_handler(signal="trie",
                                       struct=BasicNodeStruct.build_default(),
                                       flags=[HandlerSpec.flag_e.OVERRIDE])

    handlers = DEFAULT_HANDLERS()
    # Note the duplication of trie again to the default signal
    handlers += [node_handler, trie_handler, trie_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)]

    return BSS(init_specs=DEFAULT_SPECS(),
                               init_handlers=handlers,
                               sieve_fns=[])

def EXLO_PROXY_SEMANTICS():
    """
    Alternative Exclusion logic semantics, which allows separation
    of the match and act phase of rule semantics
    """
    exlo = EXLO_SEMANTICS()
    rule_sem  = ASem.ProxyRuleAbstraction().as_handler(signal=RULE_SEM_HINT,
                                                       flags=[HandlerSpec.flag_e.OVERRIDE])
    return exlo.register_handler(rule_sem)
