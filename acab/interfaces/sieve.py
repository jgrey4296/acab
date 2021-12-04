#!/usr/bin/env python3
import collections.abc as cABC
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import abc
from dataclasses import dataclass, field, InitVar
import logging as root_logger
logging = root_logger.getLogger(__name__)


import acab
config = acab.GET()

@dataclass
class AcabSieve(cABC.Container, cABC.Sized):
    """ A Generalisation of a list of functions, applied
    to a set of args, which might return results
    """

    funcs    : List[Callable]     = field(default_factory=list)
    break_fn : Optional[Callable] = field(default=None)

    def fifo(self, *args, **kwargs) -> Iterator[Any]:
        for sieve_fn in self.funcs:
            result = sieve_fn(*args, **kwargs)
            if result is None:
                continue
            yield result

    def fifo_collect(self, *args, **kwargs) -> List[Any]:
        # sieve from most to least specific
        results = []
        for sieve_fn in self.funcs:
            result = sieve_fn(*args, **kwargs)
            if result is None:
                continue
            if isinstance(result, list):
                results += result
            else:
                results.append(result)

        return results

    def filo_collect(self, *args, **kwargs) -> List[Any]:
        # sieve from most to least specific
        results = []
        for sieve_fn in self.funcs[::-1]:
            result += sieve_fn(*args, **kwargs)
            if result is None:
                continue
            if isinstance(result, list):
                results += result
            else:
                results.append(result)

        return results

    def fifo_first(self, *args, **kwargs) -> Optional[Any]:
        # sieve from most to least specific
        for sieve_fn in self.funcs:
            result += sieve_fn(*args, **kwargs)
            if result is None:
                continue
            return result

        return None

    def filo_first(self, *args, **kwargs) -> Optional[Any]:
        # sieve from most to least specific
        results = []
        for sieve_fn in self.funcs[::-1]:
            result += sieve_fn(*args, **kwargs)
            if result is None:
                continue
            return result

        return None


    def __len__(self):
        return len(self.funcs)

    def __contains__(self, value):
        return value in self.funcs
