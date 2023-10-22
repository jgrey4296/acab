#!/usr/bin/env python3
##-- imports
from __future__ import annotations
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                List, Mapping, Match, MutableMapping, Optional, Sequence,
                Set, Tuple, TypeVar, Union, cast)
from uuid import UUID

import acab.core.defaults.value_keys as DS
import acab.error.semantic as ASErr
import acab.interfaces.semantic as SI
import acab.interfaces.data as DI
from acab import types as AT
import acab
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.semantics import basic
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.modules.operators.dfs.context_walk_manager import ContextWalkManager
from acab.interfaces.context import ContextInstance_i
from acab.interfaces.bind import Bind_i

##-- end imports

logging = logmod.getLogger(__name__)
config = acab.config

CONSTRAINT_S     = DS.CONSTRAINT
NEGATION_S       = DS.NEGATION
QUERY            = DS.QUERY
QUERY_FALLBACK_S = DS.QUERY_FALLBACK
DEFAULT_SETUP_S  = config.data.DEFAULT_SETUP_METHOD
DEFAULT_UPDATE_S = config.data.DEFAULT_UPDATE_METHOD
WALK_SEM_HINT    = Sentence() << config.type.primitive.INSTRUCT << config.semantic.signals.WALK

Node          = AT.StructView
Value         = AT.Value
Structure     = AT.DataStructure
Engine        = AT.Engine
CtxSet        = AT.CtxSet
CtxIns        = AT.CtxIns
ValSem        = AT.ValueSemantics
StructSem     = AT.StructureSemantics
Handler       = AT.Handler

# TODO import
Bind = config.imports.specific.bind


# Dependent Semantics
@dataclass
class DFSSemantics(basic.StatementSemantics, SI.StatementSemantics_i):
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
        return True

    def __call__(self, instruction, semsys, ctxs=None, data=None):
        assert(isinstance(instruction, Sentence))
        if QUERY in instruction.data and bool(instruction.data[QUERY]):
            return self._query(instruction, semsys, ctxs=ctxs, data=data)

        return self._act(instruction, semsys, ctxs=ctxs, data=data)


    def _query(self, walk_spec:Sentence, semsys, ctxs=None, data=None):
        """
        DFS from @x in active ctxs
        Expects params to be a single sentence,
        with an optional @var at the head, rest are constraints

        allows binding and testing individual nodes
        """
        default: HandlerSpec[StructSem] = semsys.lookup()
        nodesem: HandlerSpec[ValSem]    = default[0].lookup()

        cwm = ContextWalkManager(walk_spec, DI.StructView(default.struct.root, self), ctxs)
        with cwm:
            for start in cwm.current:
                logging.debug("DFS Query Context Head: {}", start.current_node.value)
                # Get the current start point
                queue : tuple[CtxIns, StructView]|CtxIns = [start]
                found : set[UUID]            = set()

                # Explore with this context
                while bool(queue):
                    current   = queue.pop()
                    ctx, view = None, None
                    match current:
                        case ContextInstance_i():
                            ctx = current
                            view = current.current_node
                        case (ContextInstance_i(), DI.StructView()):
                            ctx, view = current
                        case _:
                            raise TypeError("Unknown queue member")

                    assert(ctx is not None)
                    assert(view is not None)
                    if view.node.uuid in found:
                        continue

                    found.add(view.node.uuid)
                    accessible = nodesem[0].access(view.node, None, data=data)
                    prepped    = [DI.StructView(x, self) for x in accessible]
                    unbound    = cwm.maybe_test(prepped, ctx=ctx)
                    # TODO exploration control
                    queue      += reversed(unbound)

                #endwhile
            #endfor
        #endwith
        return cwm.finished

    def _act(self, walk_spec:Sentence, semsys, ctxs=None, data=None):
        """
        instruction : Sentence(@target, $ruleset)
        from @target, dfs the trie below, running $ruleset on each node

        allows running either a pre-bound ruleset on nodes,
        or querying for a ruleset
        """
        default : HandlerSpec[StructSem]    = semsys.lookup()
        nodesem : HandlerSpec[ValSem]       = default[0].lookup()
        found   : set[UUID]                 = set()
        actions : list[ProductionStructure] = []

        assert(walk_spec[-1].type == "_:OPERATOR")
        if walk_spec[-1].is_var:
            # actions are already bound
            assert(all([walk_spec[-1][0] in x for x in ctxs]))
            actions = [x[walk_spec[-1][0]] for x in ctxs]
        else:
            # actions are specified by a patch or query
            search_result = semsys(walk_spec[-1].copy(data={DS.QUERY: True}))
            actions       = [x.current_node.value for x in search_result]


        cwm = ContextWalkManager(walk_spec, DI.StructView(default.struct.root, self), ctxs)
        with cwm:
            for start in cwm.current:
                logging.debug("DFS Act Context Head: {}", start.current_node.value)
                queue : tuple[CtxIns, StructView]|CtxIns = [start]
                while bool(queue):
                    current   = queue.pop()
                    ctx, view = None, None
                    match current:
                        case ContextInstance_i():
                            ctx  = current
                            view = current.current_node
                        case (ContextInstance_i(), DI.StructView()):
                            ctx, view = current
                        case _:
                            raise TypeError("Unknown queue member", current)

                    if view.node.uuid in found:
                        continue

                    found.add(view.node.uuid)
                    accessible = nodesem[0].access(view.node, None, data=data)
                    prepped    = [(ctx, DI.StructView(x, self)) for x in accessible]
                    queue      += reversed(prepped)

                    if default.struct.root is view.node:
                        continue

                    for action in actions:
                        spec = semsys.lookup(action)
                        # TODO use specified run form (all, sieve, etc)
                        # potentially override to force atomic rule
                        # bind current to the param in action.params
                        params : list[Value] = action.params
                        args   : list[Value] = [view.value] + walk_spec[-1].params
                        # TODO use binding
                        bind_dict = {x.key() : ctx[y] for x,y in zip(params, args)}
                        node_dict = {params[0].key() : view}

                        working_ctx = ctxs.subctx(None,
                                                val_binds=bind_dict,
                                                node_binds=node_dict)
                        # try to do the action rule
                        spec(action, semsys, ctxs=working_ctx)
                        working_ctx.run_delayed()
                        # TODO controllable entrance into subtrie
                        # using if bool(working_ctx):..
                    #endfor
                #endwhile
            #endfor
        #endwith
        return cwm.finished
