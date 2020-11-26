#!/usr/bin/env python3
#!/usr/bin/env python3
import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic


class WorkdMemoryInterface(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def to_sentences(self) -> :
        pass

    @abc.abstractmethod
    def add(self) -> None:
        pass

    @abc.abstractmethod
    def query(self) -> None:
        pass
