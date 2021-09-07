"""
Interfaces for the use of actual information, both individual,
and formed into sentences
"""
import abc
import logging as root_logger
from dataclasses import dataclass, field
from typing import (Any, Dict, List, Mapping, Match, MutableMapping, Optional,
                    Sequence, Set, Tuple, TypeVar, Union, cast)
from uuid import UUID, uuid1

from acab.abstract.config.config import AcabConfig

logging       = root_logger.getLogger(__name__)

config        = AcabConfig.Get()

Sentence      = 'Sentence'
Value_i       = 'Value_i'
AcabStatement = 'AcabStatement'

@dataclass
class Value_i(metaclass=abc.ABCMeta):

    name   : str            = field(default=None)
    value  : Any            = field(default=None)
    params : List[Value_i]  = field(default_factory=list)
    tags   : Set[Value_i]   = field(default_factory=set)
    data   : Dict[str, Any] = field(default_factory=dict)
    uuid   : UUID           = field(default_factory=uuid1)

    @staticmethod
    @abc.abstractmethod
    def safe_make(value, name, data, _type, **kwargs) -> Value_i:
        pass

    @property
    @abc.abstractmethod
    def type(self) -> Sentence:
        pass

    @abc.abstractmethod
    def bind(self, bindings: Dict[Any, Any]) -> Value_i:
        pass


    @abc.abstractmethod
    def copy(self, **kwargs) -> Value_i:
        pass



    @abc.abstractmethod
    def apply_params(self, params, data=None) -> Value_i:
        pass

    @abc.abstractmethod
    def apply_tags(self, tags, data=None) -> Value_i:
        pass


    @property
    @abc.abstractmethod
    def is_var(self) -> bool:
        pass

    @property
    @abc.abstractmethod
    def is_at_var(self) -> bool:
        pass


@dataclass
class Statement_i(metaclass=abc.ABCMeta):

    breakpoint : bool = field(init=False, default=False)
    # TODO add listener field for similar to breakpoint

    @abc.abstractmethod
    def to_word(self) -> Value_i:
        pass

    def do_break(self):
        self.breakpoint = not self.breakpoint

    @property
    def should_break(self) -> bool:
        return self.breakpoint

@dataclass
class Sentence_i(metaclass=abc.ABCMeta):

    value: List[Value_i]  = field(default_factory=list)

    @abc.abstractmethod
    def build(words, **kwargs):
        pass

    @abc.abstractmethod
    def attach_statement(self, value: Value_i) -> Sentence:
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
    def words(self):
        return self.value
