#!/usr/bin/env python3
import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from enum import Enum
from uuid import UUID, uuid1

import acab.core.defaults.value_keys as DS
import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import AcabConfig
from acab import types as AT
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import ProductionContainer
from acab.error.semantic import AcabSemanticException
from acab.interfaces.value import Sentence_i
from acab.modules.context.constraints import ConstraintCollection

config = AcabConfig()

CONSTRAINT_S     = DS.CONSTRAINT
NEGATION_S       = DS.NEGATION

CtxIns           = CtxInt.ContextInstance_i
CtxSet           = CtxInt.ContextSet_i
Constraints      = 'ConstraintCollection'
ProdComp         = ProductionComponent
ProdCon          = ProductionContainer
Operator         = 'ProductionOperator'
Value            = AT.Value
Statement        = AT.Instruction
Sen              = Sentence_i
Node             = AT.Node
StructView       = AT.StructView
ModuleFragment   = AT.ModuleFragment
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")


@dataclass
class ContextWalkManager(CtxInt.CtxManager_i):
    """ Shared State of a data structure walk, between different ctx insts """

    collect_vars  : set[str]                   = field(init=False, default_factory=set)
    constraints   : list[ConstraintCollection] = field(init=False, default_factory=list)

    _current_inst       : CtxIns               = field(init=False, default=None)
    _required_bindings  : list[str]            = field(init=False, default_factory=list)

    def __post_init__(self):
        sen = self.target_clause
        start = 0
        if sen[0].is_at_var:
            start = 1
        constraints = [ConstraintCollection(x, operators=self.ctxs._operators) for x in sen[start:]]
        self.constraints.extend(constraints)

        # preprare required_binding list
        self._required_bindings = [str(x.source) for x in constraints if x.source.is_var]

    def __enter__(self):
        # set all instances to start at node, unless start_word is an at_binding,
        # in which case get the bound node
        # handle negated behaviour
        active_list : list[CtxIns] = self.ctxs.active_list()

        if not self.target_clause[0].is_at_var:
            [x.set_current_node(self.root_node) for x in active_list]
        else:
            root_word = self.target_clause[0]
            assert(root_word.is_at_var)
            if isinstance(root_word, Sentence_i):
                root_word = root_word[0]

            [x.set_current_binding(root_word) for x in active_list]

        return self


    def __exit__(self, exc_type, exc_value, traceback):
        self.activate_ctxs()
        # collect bindings as necessary
        self.collect()
        return False


    def __iter__(self):
        return iter(self.constraints)

    @property
    def current(self) -> Iterator[CtxIns]:
        active_ctxs = self.ctxs.active_list(clear=True)
        for ctx in active_ctxs:
            self._current_inst = ctx
            yield ctx

        self.activate_ctxs()


    def maybe_test(self, possible:list[StructView], ctx=None):
        if not bool(possible):
            return []

        ctx = ctx or self._current_inst

        succs, fails = self.test(possible, ctx=ctx)
        # queue the ctxs if they are full
        full    = [x for x in succs if all([y in x for y in self._required_bindings])]
        to_fill = [x for x in succs if x not in full]
        self.queue_ctxs(full)
        # return failures with the original ctx,
        # and incomplete ctxs
        unbound = [(ctx, x) for x in fails]
        return to_fill + unbound


    def test(self, possible: list[StructView], ctx=None) -> tuple[list[CtxIns], list[StructView]]:
        """
        run a word's tests on available nodes, with current ctx instance.
        bind successes and return them
        """
        ctx = ctx or self._current_inst

        # [ctxs.test(ctxIns, accessible, x) for x in tests]

        logging.debug(f"{repr(self)}: Testing/Extending on {len(possible)} : {possible}")
        successes =  {x.source.uuid: (x, []) for x in self.constraints}
        bound_ctxs = []
        failures   = []

        # Try each node against constraints
        for view in possible:
            success = False
            for constraints in self.constraints:
                uuid = constraints.source.uuid
                try:
                    constraints.test(view, ctx)
                    successes[uuid][1].append(view)
                    success = True
                    # don't try any more constraints
                    break
                except ASErr.AcabSemanticTestFailure as err:
                    pass

            if not success:
                failures.append(view)


        for result_group in successes.values():
            constraints, succs = result_group
            bound_ctxs += ctx.progress(constraints.source,
                                       succs,
                                       sub_binds=constraints["sub_struct_binds"])

        return bound_ctxs, failures
