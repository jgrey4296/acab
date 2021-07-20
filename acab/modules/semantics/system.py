#!/usr/bin/env python3
# Main System
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces.semantic_interfaces import (AbstractionSemantics,
                                                          SemanticSystem)
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.semantics.context_container import ContextContainer


logging = root_logger.getLogger(__name__)


Sentence = 'Sentence'
config  = AcabConfig.Get()

SEM_HINT = config.prepare("Value.Structure", "SEMANTIC_HINT")()

@dataclass
class BasicSemanticSystem(SemanticSystem):
    """ A Complete semantic system """

    _default_sieve : ClassVar[List[Callable]] = [
        lambda x: x if isinstance(x, str) else None,
        lambda x: x.override if isinstance(x, SemanticSystem.HandlerOverride) else None,
        lambda x: x.data[SEM_HINT] if SEM_HINT in x.data else None,
        lambda x: x.type
    ]

    def __call__(self, instruction, ctxs=None, data=None) -> Any:
        """ Perform an instruction by mapping it to a semantics """
        if ctxs is None: # TODO: finish this
            ctxs = ContextContainer.build()

        semantics, struct = None, None
        try:
            semantics, struct = self.lookup(instruction)
            assert(semantics is not None)
            # run the semantics
            # Abstractions don't use structs
            # TODO entry hooks would go here.
            if isinstance(semantics, AbstractionSemantics):
                assert(struct is None)
                semantics(instruction, self, ctxs=ctxs, data=data)
            else:
                # but dependent semantics do
                assert(struct is not None)
                semantics(struct, instruction, ctxs=ctxs, data=data)
        except AcabSemanticException as err:
            # Semantic exceptions can be handled,
            # but others continue upwards
            # self.failure(semantics, struct, instruction, ctxs, err)
            logging.warning("Semantic Failure: {}".format(err))
        finally: # Always run exit hooks
            # TODO exit hooks would go here
            pass

        return ctxs

    def to_sentences(self) -> List[Sentence]:
        return []

