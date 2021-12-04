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
class ConfigSpec_i(cABC.Hashable):
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

    @abc.abstractmethod
    def __call__(self):
        pass



class Config_i(metaclass=abc.ABCMeta):
    pass
