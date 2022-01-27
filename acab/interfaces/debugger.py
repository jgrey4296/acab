import abc
import pdb
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)


class AcabDebugger_i(pdb.Pdb, metaclass=abc.ABCMeta):

    singleton = None

    @abc.abstractmethod
    def set_running_trace(self, *, frame=None):
        """ Start a trace going, without stopping execution """
        pass
