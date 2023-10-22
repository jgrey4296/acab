"""

"""
##-- imports
# pylint: disable=
from __future__ import annotations
import abc
import pdb
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence,
                    Tuple, TypeVar, cast)

##-- end imports


class AcabDebugger_i(pdb.Pdb):

    singleton = None

    @abc.abstractmethod
    def __bool__(self): pass

    @abc.abstractmethod
    def set_running_trace(self, *, frame:Any=None) -> None:
        """ Start a trace going, without stopping execution """
        pass
