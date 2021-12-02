"""
Interfaces for the use of actual information, both individual,
and formed into sentences
"""
import abc
import collections.abc as cABC
import logging as root_logger
from dataclasses import dataclass, field
from typing import (Any, Dict, List, Mapping, Match, MutableMapping, Optional,
                    Sequence, Set, Tuple, TypeVar, Union, cast)
from uuid import UUID, uuid1

from acab import types as AT
from acab.core.config.config import AcabConfig

logging       = root_logger.getLogger(__name__)

config        = AcabConfig.Get()

Sentence      : str = AT.Sentence
Value         : str = AT.Value
AcabStatement : str = AT.Statement

@dataclass(frozen=True)
class Value_i(metaclass=abc.ABCMeta, cABC.Hashable):

    name   : str            = field(default=None)
    value  : Any            = field(default=None)
    params : List[Value]  = field(default_factory=list)
    tags   : Set[Value]   = field(default_factory=set)
    data   : Dict[str, Any] = field(default_factory=dict)
    uuid   : UUID           = field(default_factory=uuid1)

    @staticmethod
    @abc.abstractmethod
    def safe_make(value, name, data, _type, **kwargs) -> Value:
        pass

    @property
    @abc.abstractmethod
    def type(self) -> Sentence:
        pass

    @abc.abstractmethod
    def copy(self, **kwargs) -> Value:
        pass

    @abc.abstractmethod
    def apply_params(self, params, data=None) -> Value:
        pass

    @abc.abstractmethod
    def apply_tags(self, tags, data=None) -> Value:
        pass


    @property
    @abc.abstractmethod
    def is_var(self) -> bool:
        pass

    @property
    @abc.abstractmethod
    def is_at_var(self) -> bool:
        pass


    @property
    @abc.abstractmethod
    def has_var(self) -> bool:
        pass

    @abc.abstractmethod
    def key(self) -> str:
        pass


@dataclass(frozen=True)
class Statement_i(Value_i, cABC.Sized, cABC.Container):

    breakpoint : bool = field(init=False, default=False)
    # TODO add listener field for similar to breakpoint

    @abc.abstractmethod
    def to_word(self) -> Value:
        pass

    @abc.abstractmethod
    def to_sentences(self) -> List[Sentence]:
        pass

    def do_break(self):
        self.breakpoint = not self.breakpoint

    @property
    def should_break(self) -> bool:
        return self.breakpoint

@dataclass(frozen=True)
class Sentence_i(Statement_i, cABC.Sequence):

    value: List[Value]  = field(default_factory=list)

    @abc.abstractmethod
    def build(words, **kwargs):
        pass

    @abc.abstractmethod
    def attach_statement(self, value: Value) -> Sentence:
        pass

    @abc.abstractmethod
    def detach_statement(self) -> Sentence:
        pass

    @abc.abstractmethod
    def __len__(self):
        pass
    @abc.abstractmethod
    def __iter__(self):
        pass
    @abc.abstractmethod
    def __getitem__(self, i):
        pass

    @property
    def words(self) -> List[Value]:
        return self.value
