#!/usr/bin/env python3
# Main System
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

from acab.abstract.config.config import AcabConfig
from acab.abstract.interfaces.semantic_interfaces import (SemanticSystem,
                                                          AbstractionSemantics)
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.semantics.context_container import ContextContainer

Sentence = 'Sentence'
config  = AcabConfig.Get()

@dataclass
class BasicSemanticSystem(SemanticSystem):
    """ A Complete semantic system """

    def __call__(self, instruction, data=None, override=None, ctxs=None) -> Any:
        """ Perform an instruction by mapping it to a semantics """
        if ctxs is None: # TODO: finish this
            ctxs = ContextContainer.build()

        semantics, struct = None, None
        try:
            semantics, struct = self.lookup(instruction, data=data, override=override)
            assert(semantics is not None)
            self._run_entry_hooks(semantics, struct, instruction, ctxs, data)
            # run the semantics
            # Abstractions don't use structs
            if isinstance(semantics, AbstractionSemantics):
                semantics(instruction, ctxs, self, data=data)
            else:
                # but dependent semantics do
                assert(struct is not None)
                semantics(struct, instruction, data=data, ctxs=ctxs)
        except AcabSemanticException as err:
            # Semantic exceptions can be handled,
            # but others continue upwards
            self.failure(semantics, struct, instruction, ctxs, data, err)
        finally: # Always run exit hooks
            self._run_exit_hooks(semantics, struct, instruction, ctxs, data)

        return ctxs


@dataclass
class GuaranteeSemanticSystem(SemanticSystem):
    """  """
    # Downward guarantees of what semantics may contextually rely upon
    guarantees        : Set['Handler']               = field(default_factory=list)
    # Downward expectations of what semantics must publicly provide
    expectations      : Set['SemanticUnion']         = field(init=False, default_factory=list)

    def __init__(self):
        raise NotImplementedError()
