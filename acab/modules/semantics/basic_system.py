#!/usr/bin/env python3
# Main System
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces.semantic import (AbstractionSemantics_i,
                                                          SemanticSystem_i)
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.semantics.context_set import ContextSet
from acab.abstract.interfaces.context import ContextSet_i
from acab.abstract.decorators.semantic import BuildCtxSetIfMissing, RunDelayedCtxSetActions

logging = root_logger.getLogger(__name__)

Sentence         = 'Sentence'
CtxSet           = 'ContextSet'
ModuleComponents = 'ModuleComponents'
config           = AcabConfig.Get()

SEM_HINT = config.prepare("Value.Structure", "SEMANTIC_HINT")()

# TODO make a queue structured system, where semantics return a list of
# instructions to add to the list, like printer

@dataclass
class BasicSemanticSystem(SemanticSystem_i):
    """ A Complete semantic system """

    _default_sieve : ClassVar[List[Callable]] = [
        lambda x: x if isinstance(x, str) else None,
        lambda x: x.override if isinstance(x, SemanticSystem_i.HandlerOverride) else None,
        lambda x: x.data[SEM_HINT] if SEM_HINT in x.data else None,
        lambda x: x.type
    ]

    ctx_set : ContextSet_i = field(default=ContextSet)


    @BuildCtxSetIfMissing
    @RunDelayedCtxSetActions
    def __call__(self, *instructions:List[Sentence],
                 ctxs:Optional[CtxSet]=None,
                 data:Optional[dict]=None) -> CtxSet:
        """ Perform an instruction by mapping it to a semantics """

        # Instructions passed in
        for instruction in instructions:
            if not bool(ctxs):
                logging.warning("Empty ContextSet, cannot continue received instructions")
                break

            ctxs = self.run_instruction(instruction, ctxs=ctxs, data=data)

        return ctxs

    def run_instruction(self, instruction, ctxs=None, data=None) -> Any:
        try:
            semantics, struct = self.lookup(instruction)
            assert(semantics is not None)
            logging.debug(f"Running Semantics: {semantics}")
            # TODO entry hooks would go here.

            # Abstractions use a reference to the sem system in place of a struct
            # Dependent's use a reference to a struct
            if struct is None:
                struct = self

            semantics(instruction, struct, ctxs=ctxs, data=data)

        except AcabSemanticException as err:
            # Semantic exceptions can be handled,
            # but others continue upwards
            # self.failure(semantics, struct, instruction, ctxs, err)
            logging.warning(err)
        finally: # Always run exit hooks
            # TODO exit hooks would go here
            pass

        return ctxs

    def to_sentences(self) -> List[Sentence]:
        return self.default.func.to_sentences(self.default.struct)


    def __repr__(self):
        ops = ""
        if bool(self._operator_cache):
            ops = f", {len(self._operator_cache)} operators"

        return f"({self.__class__.__name__}: {len(self.handlers)} handlers, {len(self.sieve)} sieves{ops})"
