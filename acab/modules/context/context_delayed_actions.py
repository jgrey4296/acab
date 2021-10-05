#!/opts/anaconda3/envs/ENV/python
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from uuid import UUID

from acab.abstract.decorators.util import registerOn
from acab.modules.context.context_set import ContextSet

@registerOn(ContextSet)
def do_active(self, uuids:List[UUID]):
    self._active += [x for x in uuids if x not in self._active]

@registerOn(ContextSet)
def do_fail(self, uuids:List[UUID]):
    self._failed += [x for x in uuids if x not in self._failed]

@registerOn(ContextSet)
def do_deactivate(self, uuids:List[UUID]):
    self._active = [x for x in self._active if x not in uuids]
    self._failed = [x for x in self._failed if x not in uuids]

@registerOn(ContextSet)
def do_default(self, instr, uuids:List[UUID]):
    """ Default Action if the instruction has no method """
    logging.warning(f"ContextSet bad instruction: {instr} : {uuids}")

@registerOn(ContextSet)
def do_merge(self, ctxSets):
    for ctxs in ctxSets:
        self._total.update(ctxs._total)
        self.do_active(ctxs._active)
        self.do_fail(ctxs._failed)
        self._named_sets.update(ctxs._named_sets)
