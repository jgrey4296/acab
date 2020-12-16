#!/usr/bin/env python3
# https://docs.python.org/3/library/abc.html
# from https://realpython.com/python-interface/
import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from dataclasses import dataclass, field

from acab.abstract.interfaces.util_interfaces import FlattenInterface
from acab.abstract.config.config import AcabConfig

logging            = root_logger.getLogger(__name__)

config           = AcabConfig.Get()
TYPE_INSTANCE    = config.value("Value.Structure", "TYPE_INSTANCE")
BIND             = config.value("Value.Structure", "BIND")
AT_BIND          = config.value("Value.Structure", "AT_BIND")
ANON_VALUE       = config.value("Symbols", "ANON_VALUE")
SENTENCE_TYPE      = config.value("Type.Primitive", "SENTENCE")
BIND_SYMBOL      = config.value("Symbols", "BIND")
AT_BIND_SYMBOL   = config.value("Symbols", "AT_BIND")
TYPE_BOTTOM_NAME = config.value("Data", "TYPE_BOTTOM_NAME")
UUID_CHOP        = bool(int(config.value("Print.Data", "UUID_CHOP")))
FALLBACK_MODAL   = config.value("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])


SENTENCE_TYPE      = config.value("Type.Primitive", "SENTENCE")


Sentence      = 'Sentence'
AcabValue     = 'AcabValue'
AcabStatement = 'AcabStatement'

@dataclass
class ValueInterface(FlattenInterface, metaclass=abc.ABCMeta):

    name : str               = field(default=None)
    value : Any              = field(default=None)
    params : List[AcabValue] = field(default_factory=list)
    tags : Set[str]          = field(default_factory=set)
    data : Dict[str, Any]    = field(default_factory=dict)
    uuid : UUID              = field(default_factory=uuid1)

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
    def type(self) -> Sentence:
        pass

    @abc.abstractmethod
    def bind(self, bindings: Dict[Any, Any]) -> AcabValue:
        pass


    @abc.abstractmethod
    def copy(self) -> AcabValue:
        pass


@dataclass
class SentenceInterface(FlattenInterface, metaclass=abc.ABCMeta):

    words : List[ValueInterface]  = field(default_factory=list)

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
