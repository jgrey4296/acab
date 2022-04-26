#!/opts/anaconda3/envs/ENV/python
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID

if TYPE_CHECKING:
    # tc only imports
    pass

logging = logmod.getLogger(__name__)

from acab.core.util.decorators.util import registerOn
from acab.modules.context.context_set import ContextSet


@registerOn(ContextSet)
def do_active(self, uuids:list[UUID]):
    self._active += [x for x in uuids if x not in self._active]

@registerOn(ContextSet)
def do_fail(self, uuids:list[UUID]):
    self._failed += [x for x in uuids if x not in self._failed]

@registerOn(ContextSet)
def do_deactivate(self, uuids:list[UUID]):
    self._active = [x for x in self._active if x not in uuids]
    self._failed = [x for x in self._failed if x.ctx.uuid not in uuids]

@registerOn(ContextSet)
def do_default(self, instr, uuids:list[UUID]):
    """ Default Action if the instruction has no method """
    logging.warning(f"ContextSet bad instruction: {instr} : {uuids}")

@registerOn(ContextSet)
def do_merge(self, ctxSets):
    logging.debug("Merging Contexts into parent")
    for ctxs in ctxSets:
        self._total.update(ctxs._total)
        self.do_active(ctxs._active)
        self.do_fail(ctxs._failed)
        self._named_sets.update(ctxs._named_sets)
