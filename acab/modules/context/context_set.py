from __future__ import annotations

import logging as logmod
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, replace
from enum import Enum
from uuid import UUID, uuid1

import acab.core.defaults.value_keys as DS
import acab.interfaces.context as CtxInt
import acab.interfaces.value as VI
from acab import AcabConfig
from acab import types as AT
from acab.core.util.delayed_commands import DelayedCommands_i
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import ProductionContainer
from acab.error.context import AcabContextException
from acab.interfaces.value import Sentence_i
from acab.modules.context.constraints import ConstraintCollection
from acab.modules.context.context_instance import ContextInstance
from acab.modules.context.context_meta import ContextMeta

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
Sen              = AT.Sentence
Node             = AT.StructView
ModuleFragment   = AT.ModuleFragment
NamedCtxSet      = "NamedCtxSet"

DELAYED_E = Enum("Delayed Instruction Set", "ACTIVE FAIL DEACTIVATE CLEAR MERGE")

NamedCtxSet      = CtxInt.NamedCtxSet_d
ContextFailState = CtxInt.ContextFailState_d

@dataclass
class ContextSet(CtxInt.ContextSet_i, DelayedCommands_i, metaclass=ContextMeta):

    # TODO make operators a stack
    # operators are just the results of a prior query
    _operators           : CtxIns                 = field(default=None)

    # For nesting ctxsets
    _parent              : None|CtxSet            = field(default=None)

    _total               : dict[UUID, CtxIns]     = field(default_factory=dict)
    _active              : list[UUID]             = field(default_factory=list)

    _failed              : list[ContextFailState] = field(init=False, default_factory=list)
    _named_sets          : dict[Any, NamedCtxSet] = field(init=False, default_factory=dict)
    _uuid                : UUID                   = field(init=False, default_factory=uuid1)

    delayed_e            : Enum                   = field(init=False, default=DELAYED_E)
    instance_constructor : CtxIns                 = field(init=False, default=ContextInstance)


    def subctx(self, selection:None|list[CtxIns|UUID]=None, *, val_binds:dict[str,Value]=None, node_binds:dict[str, Node]=None) -> CtxSet:
        """
        Subctx the given instances,
        *and* bind the given data as part of that subctx.
        (Intended to avoid manual mutx creation, etc)
        """
        # TODO if selection == root? then use empty root context
        # TODO selection:slice|List[UUIDs|CtxIns]|bool|None
        val_binds  = val_binds or {}
        node_binds = node_binds or {}
        selection  = selection or self._active

        if all([isinstance(x, CtxIns) for x in selection]):
            selection     = [x.uuid for x in selection]

        # Get anything based on specified selection
        if bool(self.active_list()):
            selection = [x.uuid for x in self.active_list() if x._lineage.intersection(selection)]
        # Construct the mapping for the subctx, while binding
        if not bool(selection):
            initial       = self.instance_constructor().progress(val_binds, node_binds)
            obj_selection = {x.uuid : x for x in initial}
        else:
            rebound = [self._total[x].progress(val_binds, node_binds)[0] for x in selection]
            obj_selection = {x.uuid : x for x in rebound}

        # Remove the selection from the self's active list, as its now in the child
        self._active = [x for x in self._active if x not in selection]

        assert(all([isinstance(x, CtxIns) for x in obj_selection.values()]))
        assert(all([isinstance(x, UUID) for x in selection]))
        subctx = ContextSet(_operators=self._operators,
                            _parent=self,
                            _total=obj_selection,
                            _active=list(obj_selection.keys()))

        # register merge of subctx into self (controllable param)
        self.delay(self.delayed_e.MERGE, val=subctx)
        return subctx

    def __post_init__(self):
        logging.debug("ContextSet Created")
        if not bool(self._total):
            initial = self.instance_constructor()
            self._total[initial.uuid] = initial
            self._active.append(initial.uuid)


    def __hash__(self):
        return hash(self._uuid)
    def __len__(self):
        return len(self._active)

    def __getitem__(self, index, *indices) -> CtxIns|CtxSet:
        """ Access the active CtxSet, for:
        a specifc instance (by int index),
        a subselection (using a slice),
        a named/continuation set (using the instruction that named it)
        """
        result = None
        match index:
            case int():
                ctx_uuid = self._active[index]
                result   = self._total[ctx_uuid]
            case UUID():
                result = self._total[index]
            case slice():
                result = self._active[index]
            case list():
                result = [self.__getitem__(x) for x in index]
            case VI.Instruction_i() if index in self._named_sets:
                result = self._named_sets[index].uuids
            case _:
                raise AcabContextException(f"Unrecognised arg to getitem: {index}")

        if isinstance(result, list):
            result = self.subctx(result)

        return result

    def __bool__(self):
        return bool(self._active)

    def __repr__(self):
        return f"(CtxSet: Active:{len(self._active)} Failed:{len(self._failed)} Total:{len(self._total)})"

    def __contains__(self, key: CtxIns|UUID|Statement):
        match key:
            case CtxIns():
                return key in self._active
            case UUID():
                return key.uuid in self._active
            case VI.Instruction_i():
                return key in self._named_sets
            case _:
                raise AcabContextException(f"Unrecognised arg to contains: {index}")

    def __iter__(self):
        for uuid in self._active:
            yield self._total[uuid]

    def fail(self, instance: CtxIns, word: Value, node: Node, query:Sen):
        """ Record a failure, the query sentence that failed,
        and the word that it failed on """
        # add failure details to the instance, of word and query clause
        logging.debug(f"{repr(self)}: Failing: {node}")

        fail_state = ContextFailState(instance,
                                      query,
                                      word,
                                      node)
        self._failed.append(fail_state)

    def push(self, ctxs:UUID|CtxIns|list[CtxIns|UUID]):
        if not isinstance(ctxs, list):
            ctxs = [ctxs]

        if all([isinstance(x, UUID) for x in ctxs]):
            assert(all([x in self._total for x in ctxs]))
            self._active += ctxs

        else:
            # Add to set
            assert(not any([x.uuid in self._total for x in ctxs]))
            self._total.update({x.uuid: x for x in ctxs})
            self._active += [x.uuid for x in ctxs]



    def pop(self, top=False) -> CtxIns:
        """ Get a copy of the active contexts,
        so ctxs can be modified as semantics go
        """
        if top:
            return self._total[self._active.pop()]
        else:
            return self._total[self._active.pop(0)]


    def active_list(self, clear=False) -> list[CtxIns]:
        the_list = [self._total[x] for x in self._active]
        if clear:
            self._active = []

        return the_list

    def failed_list(self) -> list[CtxIns]:
        return self._failed


    def set_parent(self, parent:CtxSet):
        self._parent = parent



    def build_named_set(self, inst, uuids:list[UUID]):
        assert(inst not in self._named_sets)
        self._named_sets[inst] = NamedCtxSet(inst, uuids)
        return self._named_sets[inst]


