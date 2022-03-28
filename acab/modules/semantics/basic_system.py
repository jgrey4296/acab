#!/usr/bin/env python3
# Main System
from __future__ import annotations
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.decorators.semantic import (BuildCtxSetIfMissing,
                                               RunDelayedCtxSetActions)
from acab.interfaces.semantic import (StatementSemantics_i,
                                      SemanticSystem_i)
from acab.interfaces.handler_system import HandlerOverride
from acab.core.semantics import basic

from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_set import ContextSet

logging = root_logger.getLogger(__name__)

Sentence         = AT.Sentence
CtxSet           = AT.CtxSet
ModuleComponents = AT.ModuleComponents
config           = AcabConfig()

SEM_HINT = config.prepare("Value.Structure", "SEMANTIC_HINT")()

# TODO make a queue structured system, where semantics return a list of
# instructions to add to the list, like printer

@dataclass
class BasicSemanticSystem(basic.SemanticSystem, SemanticSystem_i):
    """ A Complete semantic system """

    _default_sieve : ClassVar[list[Callable]] = [
        lambda x: x if isinstance(x, str) else None,
        lambda x: x.signal if isinstance(x, HandlerOverride) else None,
        lambda x: str(x.data[SEM_HINT]) if SEM_HINT in x.data else None,
        lambda x: str(x.type)
    ]

    ctx_set : CtxSet = field(default=ContextSet)

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
            data   = {}
            spec   = self.lookup(instruction)
            # TODO verify instruction against semantics
            assert(spec is not None)
            struct = spec.struct
            logging.debug(f"Firing Semantics: {spec}")
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
        finally: # Always run exit hooks
            # TODO exit hooks would go here
            pass

        return ctxs

    def to_sentences(self) -> list[Sentence]:
        # TODO run to_sentences for *all* registered structure semantic specs?
        default = self.lookup()
        return default[0].to_sentences(default.struct)


    def __repr__(self):
        ops = ""
        if bool(self._operator_cache):
            ops = f", operators={len(self._operator_cache)}"

        return f"{self.__class__.__name__}(handlers={len(self.handler_specs)}, sieve={len(self.sieve)}{ops})"

    @staticmethod
    def from_sentences(self, sens):
        raise NotImplementedError()

    def to_word(self):
        raise NotImplementedError()
