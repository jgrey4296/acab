#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

import logging as root_logger
from uuid import UUID

from acab import types as AT
import acab.interfaces.semantic as SI
import acab.error.semantic_exception as ASErr
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.data.value import Instruction, Sentence
from acab.modules.operators.dfs.context_walk_manager import ContextWalkManager

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

CONSTRAINT_S     = config.prepare("Value.Structure", "CONSTRAINT")()
DEFAULT_SETUP_S  = config.prepare("Data", "DEFAULT_SETUP_METHOD")()
DEFAULT_UPDATE_S = config.prepare("Data", "DEFAULT_UPDATE_METHOD")()
NEGATION_S       = config.prepare("Value.Structure", "NEGATION")()
QUERY            = config.prepare("Value.Structure", "QUERY")()
QUERY_FALLBACK_S = config.prepare("Value.Structure", "QUERY_FALLBACK")()
WALK_SEM_HINT    = Sentence.build([config.prepare("Module.DFSWalk", "WALK_SEM_HINT")()])

Node          = AT.Node
Value         = AT.Value
Structure     = AT.DataStructure
Engine        = AT.Engine
CtxSet        = AT.CtxSet
ValSem        = AT.ValueSemantics
StructSem     = AT.StructureSemantics
Handler       = AT.Handler

# Dependent Semantics
@dataclass
class WalkTrieSemantics(SI.StatementSemantics_i):
    """
    Instruction for walking all nodes in a struct,
    instruction specifies rules to run on each node

    AS QUERY:
    a.b.$x?
    // From a node
    @x ᛦ $y(::constraint)?
    // from Root
    ᛦ $y(::constraint)?

    AS ACTION:
    a.b.$x?
    a.$rules(λcollect)?

    @x ᛦ $rule
    """

    signal : str = field(default=WALK_SEM_HINT)

    def verify(self, instruction):
        pass

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        if QUERY in instruction[-1].data and bool(instruction[-1].data[QUERY]):
            return self._query(instruction, semsys, ctxs=ctxs, data=data)

        return self._act(instruction, semsys, ctxs=ctxs, data=data)


    def _query(self, walk_spec, semsys, ctxs=None, data=None):
        """
        DFS from @x in active ctxs
        Expects params to be a single sentence,
        with an optional @var at the head, rest are constraints

        """
        default: HandlerSpec[StructSem] = semsys.lookup()
        nodesem: HandlerSpec[ValSem]    = default[0].lookup()

        with ContextWalkManager(walk_spec, default.struct.root, ctxs) as cwm:
            for queue in cwm.active:
                found      : Set[UUID]  = set()

                while bool(queue):
                    current      = queue.pop(0)
                    if current.uuid in found:
                        continue

                    found.add(current.uuid)
                    accessible   = nodesem[0].access(current, None, data)
                    queue       += accessible
                    cwm.test_and_update(accessible)


    def _act(self, walk_spec, semsys, ctxs=None, data=None):
        """
        instruction : Sentence(@target, $ruleset)
        from @target, dfs the trie below, running $ruleset on each node

        """
        default : HandlerSpec[StructSem] = semsys.lookup()
        nodesem : HandlerSpec[ValSem]    = default[0].lookup()
        found   : Set[UUID]              = set()


        with ContextWalkManager(walk_spec, default.struct.root, ctxs) as cwm:
            # for queue in cwm.active(mutable=True) ?
            # queue::List[Node]
            for queue in cwm.active:
                action : Union['Value', 'Sentence'] = cwm.current[walk_spec[-1]]
                if isinstance(action, Sentence):
                    raise NotImplementedError()

                while bool(queue):
                    current      = queue.pop(0)
                    if current.uuid in found:
                        continue

                    found.add(current.uuid)
                    # TODO use specified run form (all, sieve, etc)
                    # potentially override to force atomic rule
                    spec = semsys.lookup(action)
                    # bind current to the param in action.params
                    params : List[Value] = action.params
                    args   : List[Value] = [current.value] + walk_spec[-1].params
                    bind_dict = {x.key() : cwm._current_inst[y] for x,y in zip(params, args)}
                    node_dict = {params[0].key() : current}

                    working_ctx = ctxs.subctx(None,
                                              val_binds=bind_dict,
                                              node_binds=node_dict)
                    # try to do the action rule
                    spec(action, semsys, ctxs=working_ctx)
                    working_ctx.run_delayed()
                    # TODO controllable entrance into subtrie
                    # using if bool(working_ctx):..
                    accessible   = nodesem[0].access(current, None, data)
                    queue       += accessible
