"""

"""
##-- imports
from __future__ import annotations
import abc
import collections.abc as cABC
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Protocol, Sequence, Tuple,
                    TypeAlias, TypeVar, cast)

import acab

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    # tc only imports
    pass

##-- end imports

logging = logmod.getLogger(__name__)
config  = acab.config

from acab import types as AT
GenFunc : TypeAlias = AT.fns.GenFunc
T                   = TypeVar('T')

@dataclass
class _AcabSieve_d(Generic[T]):
    """ A Generalisation of a list of functions, applied
    to a set of args, which might return results
    """
    funcs    : list[GenFunc]  = field(default_factory=list)
    break_fn : None | GenFunc = field(default=None)

class AcabSieve(cABC.Container[GenFunc], _AcabSieve_d[T]):

    def fifo(self, *args:Any, **kwargs:Any) -> Iterator[None|T]:
        for sieve_fn in self.funcs:
            result = sieve_fn(*args, **kwargs)
            if result is None:
                continue
            yield result
        yield None

    def fifo_collect(self, *args:Any, **kwargs:Any) -> list[T]:
        # sieve from most to least specific
        results : list[T] = []
        for sieve_fn in self.funcs:
            result = sieve_fn(*args, **kwargs)
            if result is None:
                continue
            if isinstance(result, list):
                results += result
            else:
                results.append(result)

        return results

    def filo_collect(self, *args:Any, **kwargs:Any) -> list[T]:
        # sieve from most to least specific
        results : list[T] = []
        for sieve_fn in self.funcs[::-1]:
            result = sieve_fn(*args, **kwargs)
            if result is None:
                continue
            if isinstance(result, list):
                results += result
            else:
                results.append(result)

        return results

    def fifo_first(self, *args:Any, **kwargs:Any) -> None | T:
        # sieve from most to least specific
        for sieve_fn in self.funcs:
            result = sieve_fn(*args, **kwargs)
            if result is None:
                continue
            return result

        return None

    def filo_first(self, *args:Any, **kwargs:Any) -> None | T:
        # sieve from most to least specific
        for sieve_fn in self.funcs[::-1]:
            result = sieve_fn(*args, **kwargs)
            if result is None:
                continue
            return result

        return None

    def __len__(self) -> int:
        return len(self.funcs)

    def __contains__(self, value:Any) -> bool:
        return value in self.funcs
