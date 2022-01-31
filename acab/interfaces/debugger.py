import abc
import pdb
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence,
                    Tuple, TypeVar, cast)


class AcabDebugger_i(pdb.Pdb, metaclass=abc.ABCMeta):

    singleton = None

    @abc.abstractmethod
    def set_running_trace(self, *, frame=None):
        """ Start a trace going, without stopping execution """
        pass
