#!/usr/bin/env python3
# https://docs.python.org/3/library/abc.html
# from https://realpython.com/python-interface/
import abc
import collections.abc as cABC
from dataclasses import dataclass, field
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from enum import Enum


@dataclass
class ConfigSpec_i(cABC.Hashable, cABC.Callable):
    """ Dataclass to describe a config file value,
    and any transforms it needs prior to use """

    section : str                = field()
    key     : Optional[str]      = field(default=None)
    actions : List[Enum]         = field(default_factory=list)
    as_list : bool               = field(default=False)
    as_dict : bool               = field(default=False)
    as_enum : bool               = field(default=False)
    as_bool : bool               = field(default=False)

    def __hash__(self):
        return hash(f"{self.section}:{self.key}")


class Config_i(cABC.Callable, cABC.Collection):
    @abc.abstractstaticmethod
    def Get(*paths: str, hooks=None):
        pass

    @abc.abstractmethod
    def prepare(self, *args, **kwargs):
        pass

    @abc.abstractmethod
    def read(self, paths: List[str]):
        pass

    @abc.abstractmethod
    def override(self, spec: ConfigSpec_i, value):
        pass
