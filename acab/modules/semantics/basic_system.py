#!/usr/bin/env python3
# Main System
from __future__ import annotations

import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.defaults.value_keys as DS
from acab import types as AT
from acab import AcabConfig
from acab.core.semantics import basic
from acab.core.util.decorators.semantic import (BuildCtxSetIfMissing,
                                                RunDelayedCtxSetActions)
from acab.error.semantic import AcabSemanticException
from acab.interfaces.context import ContextSet_i
from acab.interfaces.handler_system import HandlerOverride
from acab.interfaces.semantic import SemanticSystem_i, StatementSemantics_i

logging = logmod.getLogger(__name__)

Sentence         = AT.Sentence
CtxSet           = AT.CtxSet
ModuleFragment   = AT.ModuleFragment
config           = AcabConfig()

ContextSet = config.prepare("Imports.Targeted", "context", actions=[config.actions_e.IMCLASS], args={"interface": ContextSet_i})()
SEM_HINT   = DS.SEMANTIC_HINT

# TODO make a queue structured system, where semantics return a list of
# instructions to add to the list, like printer

@dataclass(repr=False)
class BasicSemanticSystem(basic.SemanticSystem, SemanticSystem_i):
    """ A Complete semantic system """

    _default_sieve : ClassVar[list[Callable]] = [
        lambda x: x if isinstance(x, str) else None,
        lambda x: x.signal if isinstance(x, HandlerOverride) else None,
        lambda x: str(x.data[SEM_HINT]) if SEM_HINT in x.data else None,
        lambda x: x.type
    ]

    ctx_set : Type[CtxSet] = field(default=ContextSet)

    @BuildCtxSetIfMissing
    @RunDelayedCtxSetActions
    def __call__(self, *instructions:list[Sentence],
                 ctxs:None|CtxSet=None) -> CtxSet:
        """ Perform an instruction by mapping it to a semantics """

        # Instructions passed in
        for instruction in instructions:
            if not bool(ctxs):
                logging.warning("Empty ContextSet, cannot continue received instructions")
                break

            ctxs = self.run_instruction(instruction, ctxs=ctxs)

        return ctxs

    def run_instruction(self, instruction, ctxs=None) -> Any:
        try:
            # TODO Entry hooks, like TypeExpansion, would go here?
            data   = {}
            spec   = self.lookup(instruction)
            # TODO verify instruction against semantics
            assert(spec is not None)
            struct = spec.struct
            logging.info("Firing Semantics: {} : {!r} : {!r}", spec, instruction, ctxs)
            # StructSems's use a reference to a struct
            # StatementSems use a reference to the sem system in place of a struct
            if struct is None:
                struct = self

            if isinstance(instruction, HandlerOverride):
                data.update(instruction.data)
                instruction = instruction.value

            result = spec(instruction, struct, ctxs=ctxs, data=data)

        except AcabSemanticException as err:
            # Semantic exceptions can be handled,
            # but others continue upwards
            # self.failure(semantics, struct, instruction, ctxs, err)
            logging.warning(err)
            raise err
        finally: # Always run exit hooks
            # TODO exit hooks would go here
            pass

        return ctxs

    def to_sentences(self) -> list[Sentence]:
        # TODO run to_sentences for *all* registered structure semantic specs?
        default = self.lookup()
        return default[0].to_sentences(default.struct)


