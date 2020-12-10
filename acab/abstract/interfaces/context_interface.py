#!/usr/bin/env python3

import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

class ContextInterface(metaclass=abc.ABCMeta):
    """  """

    @abc.abstractmethod
    def append(self) -> Any:
        pass

    @abc.abstractmethod
    def fail(self) -> Any:
        pass

    @abc.abstractmethod
    def clear(self) -> Any:
        pass

    @abc.abstractmethod
    def collapse(self) -> Any:
        pass

    @abc.abstractmethod
    def group_by_type(self) -> Any:
        pass

    @abc.abstractmethod
    def promote_failures(self) -> Any:
        pass

    @abc.abstractmethod
    def demote_failures(self) -> Any:
        pass

    @abc.abstractmethod
    def force_node_position(self) -> Any:
        pass

    @abc.abstractmethod
    def rebind_across_contexts(self) -> Any:
        pass
