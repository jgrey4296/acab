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

logging            = root_logger.getLogger(__name__)

config           = AcabConfig.Get()

Sentence      = 'Sentence'
AcabValue     = 'AcabValue'
AcabStatement = 'AcabStatement'

@dataclass
class Value_i(metaclass=abc.ABCMeta):

    name   : str             = field(default=None)
    value  : Any             = field(default=None)
    params : List[AcabValue] = field(default_factory=list)
    tags   : Set[str]        = field(default_factory=set)
    data   : Dict[str, Any]  = field(default_factory=dict)
    uuid   : UUID            = field(default_factory=uuid1)

    @property
    @abc.abstractmethod
    def type(self) -> Sentence:
        pass

    @abc.abstractmethod
    def bind(self, bindings: Dict[Any, Any]) -> AcabValue:
        pass


    @abc.abstractmethod
    def copy(self, **kwargs) -> AcabValue:
        pass


@dataclass
class Sentence_i(metaclass=abc.ABCMeta):

    value: List[Value_i]  = field(default_factory=list)

    @abc.abstractmethod
    def build(words, **kwargs):
        pass

    @abc.abstractmethod
    def attach_statement(self, value: AcabValue) -> Sentence:
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
