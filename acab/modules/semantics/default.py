##-- imports
from __future__ import annotations
import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.interfaces.semantic as SI
import acab.modules.semantics.statements as ASem
import acab
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.util.part_implementations.handler_system import (Handler,
                                                                HandlerSpec)
from acab.interfaces.value import ValueFactory as VF
from acab.modules.context import context_delayed_actions
from acab.modules.semantics.basic_system import BasicSemanticSystem as BSS
from acab.modules.semantics.values import (BasicNodeSemantics,
                                           ExclusionNodeSemantics)
from acab.modules.structures.trie.default import (DEFAULT_TRIE,
                                                  DEFAULT_TRIE_SPEC)
from acab.modules.structures.trie.semantics import FlattenBreadthTrieSemantics

##-- end imports

logging    = logmod.getLogger(__name__)
config     = acab.config

DEFAULT_HANDLER_SIGNAL = config.handler.system.DEFAULT_SIGNAL
INSTRUCT_SEN     = VF.sen() << config.any_of().types.primitive.INSTRUCT
CONTAINER_SEN    = INSTRUCT_SEN << config.any_of().types.primitive.CONTAINER
STRUCT_SEN       = INSTRUCT_SEN << config.any_of().types.primitive.STRUCTURE

QUERY_SIGNAL     = CONTAINER_SEN << config.any_of().types.primitive.QUERY
ACTION_SIGNAL    = CONTAINER_SEN << config.any_of().types.primitive.ACTION
TRANSFORM_SIGNAL = CONTAINER_SEN << config.any_of().types.primitive.TRANSFORM
RULE_SIGNAL      = STRUCT_SEN    << config.any_of().types.primitive.RULE
ATOM_SIGNAL      = VF.sen()      << config.data.TYPE_BASE

query_spec  = BSS.Spec(QUERY_SIGNAL).spec_from(SI.StatementSemantics_i)
action_spec = BSS.Spec(ACTION_SIGNAL).spec_from(SI.StatementSemantics_i)
rule_spec   = BSS.Spec(RULE_SIGNAL).spec_from(SI.StatementSemantics_i)
trans_spec  = BSS.Spec(TRANSFORM_SIGNAL).spec_from(SI.StatementSemantics_i)
cont_spec   = BSS.Spec(CONTAINER_SEN).spec_from(SI.StatementSemantics_i)

def DEFAULT_SPECS():
    """
    Return the basic specs a semantic system needs to respond to
    """
    logging.debug("Constructing Default Semantic Specs")
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
    logging.debug("Constructing Default Semantic Handlers")
    node_handler, trie_handler = DEFAULT_TRIE()

    query_handler   = ASem.QueryAbstraction().as_handler(signal=QUERY_SIGNAL)
    action_handler  = ASem.ActionAbstraction().as_handler(signal=ACTION_SIGNAL)
    rule_handler    = ASem.AtomicRuleAbstraction().as_handler(signal=RULE_SIGNAL)
    trans_handler   = ASem.TransformPlusAbstraction().as_handler(signal=TRANSFORM_SIGNAL)
    cont_handler    = ASem.ContainerAbstraction().as_handler(signal=CONTAINER_SEN)

    return [cont_handler, query_handler, action_handler, rule_handler,
            trans_handler, node_handler, trie_handler,
            trie_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)]

def default_handlers_from_specs():
    """
    An alternative way to build the default handlers,
    using the specs themselves
    """
    logging.debug("Constructing Default Semantic Handlers From default Specs")
    node_spec, trie_spec       = DEFAULT_TRIE_SPEC()
    node_handler, trie_handler = DEFAULT_TRIE()

    return [
        query_spec.on(ASem.QueryAbstraction()),
        action_spec.on(ASem.ActionAbstraction()),
        rule_spec.on(ASem.AtomicRuleAbstraction()),
        trans_spec.on(ASem.TransformPlusAbstraction()),
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
    logging.debug("Constructing Default Semantic System")
    return BSS(init_specs=DEFAULT_SPECS(),
               init_handlers=DEFAULT_HANDLERS())


def EXLO_SEMANTICS():
    """
    Instead of simple node semantics, builds a basic semantic system
    with exclusion logic semantics for nodes
    """
    logging.debug("Constructing EXLO Semantic System")
    node_handler = ExclusionNodeSemantics().as_handler(signal=ATOM_SIGNAL)
    trie_sem     = FlattenBreadthTrieSemantics(init_handlers=[node_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)])


    trie_handler = trie_sem.as_handler(signal="trie",
                                       struct=BasicNodeStruct.build_default(),
                                       flags=[HandlerSpec.flag_e.OVERRIDE])

    handlers = DEFAULT_HANDLERS()
    # Note the duplication of trie again to the default signal
    handlers += [node_handler, trie_handler, trie_handler.as_handler(signal=DEFAULT_HANDLER_SIGNAL)]

    return BSS(init_specs=DEFAULT_SPECS(),
               init_handlers=handlers)

def EXLO_PROXY_SEMANTICS():
    """
    Alternative Exclusion logic semantics, which allows separation
    of the match and act phase of rule semantics
    """
    logging.debug("Constructing EXLO Semantics with Proxy Rule handler")
    exlo = EXLO_SEMANTICS()
    rule_sem  = ASem.ProxyRuleAbstraction().as_handler(signal=RULE_SIGNAL,
                                                       flags=[HandlerSpec.flag_e.OVERRIDE])
    return exlo.register_handler(rule_sem)
