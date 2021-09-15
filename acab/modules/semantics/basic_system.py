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

    def __call__(self, *instructions:List[Sentence],
                 ctxs:Optional[CtxSet]=None,
                 data:Optional[dict]=None) -> CtxSet:
        """ Perform an instruction by mapping it to a semantics """
        if ctxs is None:
            # Default, doesn't include operators
            ctxs = self.build_ctxset()

        # Instructions passed in
        for instruction in instructions:
            if not bool(ctxs):
                logging.warning("Empty ContextSet, cannot continue received instructions")
                break

            ctxs = self.run_instruction(instruction, ctxs=ctxs, data=data)

        # TODO otherwise run contextset continuations

        ctxs.run_delayed()
        return ctxs

    def run_instruction(self, instruction, ctxs=None, data=None) -> Any:
        semantics, struct = None, None
        try:
            semantics, struct = self.lookup(instruction)
            assert(semantics is not None)
            logging.debug(f"Running Semantics: {semantics}")
            # run the semantics
            # Abstractions don't use structs
            # TODO entry hooks would go here.
            if isinstance(semantics, AbstractionSemantics_i):
                assert(struct is None)
                semantics(instruction, self, ctxs=ctxs, data=data)
            else:
                # but dependent semantics do
                assert(struct is not None)
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
