#!/usr/bin/env python3
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
from acab.core.config.config import AcabConfig
from acab.core.data.acab_struct import BasicNodeStruct
from acab.core.semantics import basic
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.modules.operators.dfs.context_walk_manager import ContextWalkManager
from acab.interfaces.context import ContextInstance_i

logging = logmod.getLogger(__name__)
config = AcabConfig()

CONSTRAINT_S     = DS.CONSTRAINT
NEGATION_S       = DS.NEGATION
QUERY            = DS.QUERY
QUERY_FALLBACK_S = DS.QUERY_FALLBACK
DEFAULT_SETUP_S  = config.attr.Data.DEFAULT_SETUP_METHOD
DEFAULT_UPDATE_S = config.attr.Data.DEFAULT_UPDATE_METHOD
WALK_SEM_HINT    = Sentence() << config.attr.Type.Primitive.INSTRUCT << config.attr.Semantic.Signals.WALK

Node          = AT.StructView
Value         = AT.Value
Structure     = AT.DataStructure
Engine        = AT.Engine
CtxSet        = AT.CtxSet
CtxIns        = AT.CtxIns
ValSem        = AT.ValueSemantics
StructSem     = AT.StructureSemantics
Handler       = AT.Handler

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

        """
        default: HandlerSpec[StructSem] = semsys.lookup()
        nodesem: HandlerSpec[ValSem]    = default[0].lookup()

        cwm = ContextWalkManager(walk_spec, DI.StructView(default.struct.root, self), ctxs)
        with cwm:
            for start in cwm.current:
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

        return cwm.finished

    def _act(self, walk_spec:Sentence, semsys, ctxs=None, data=None):
        """
        instruction : Sentence(@target, $ruleset)
        from @target, dfs the trie below, running $ruleset on each node

        """
        default : HandlerSpec[StructSem] = semsys.lookup()
        nodesem : HandlerSpec[ValSem]    = default[0].lookup()
        found   : set[UUID]              = set()

        # TODO handle λrule.sentence.queries.$x?

        cwm = ContextWalkManager(walk_spec, DI.StructView(default.struct.root, self), ctxs)
        with cwm:
            for start in cwm.current:
                queue : tuple[CtxIns, StructView]|CtxIns = [start]
                # TODO: this should use binding?
                action : 'Value|Sentence' = start[walk_spec[-1][0]]
                spec = semsys.lookup(action)
                if isinstance(action, Sentence):
                    # action is a sentence path,
                    # not an actual action
                    raise NotImplementedError()

                assert(bool(action.params))
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
                    accessible = nodesem[0].access(view.node, None, data=data)
                    prepped    = [(ctx, DI.StructView(x, self)) for x in accessible]
                    queue      += reversed(prepped)

        return cwm.finished
