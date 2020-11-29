#!/usr/bin/env python3
# https://docs.python.org/3/library/abc.html
# from https://realpython.com/python-interface/
import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

class ValueInterface(metaclass=abc.ABCMeta):

    @property
    @abc.abstractmethod
    def name(self) -> str:
        pass

    @property
    @abc.abstractmethod
    def value(self) -> Any:
        pass

    @property
    @abc.abstractmethod
    def type(self) -> 'Sentence':
        pass

    @abc.abstractmethod
    def to_sentences(self) -> List['Sentence']:
        pass

    @abc.abstractmethod
    def bind(self, bindings: Dict[Any, Any]) -> 'AcabValue':
        pass


    @abc.abstractmethod
    def copy(self) -> 'AcabValue':
        pass

class SentenceInterface(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def attach_statement(self, value: 'AcabValue') -> 'Sentence':
        pass

    @abc.abstractmethod
    def detach_statement(self) -> 'Sentence':
        pass

    @abc.abstractmethod
    def __getitem__(self, i) -> 'AcabValue':
        pass
