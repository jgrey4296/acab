#!/usr/bin/env python3
# from https://realpython.com/python-interface/
# pylint: disable=multiple-statements
from __future__ import annotations
import abc
import collections.abc as cABC
from dataclasses import dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator, Container,
                    Mapping, Match, MutableMapping, Protocol, Sequence, Tuple,
                    TypeAlias, TypeVar, cast, runtime_checkable)
from logging import Logger, getLogger


from acab import types as AT

GenFunc = AT.fns.GenFunc

@dataclass
class ConfigSpec_d:
    """ Dataclass to describe a config file value,
    and any transforms it needs prior to use """

    section     : str        = field()
    key         : None | str = field(default=None)
    actions     : list[Enum] = field(default_factory=list)
    action_args : list[Any]  = field(default_factory=list)
    as_list     : bool       = field(default=False)
    as_dict     : bool       = field(default=False)
    as_enum     : bool       = field(default=False)
    as_bool     : bool       = field(default=False)


    def __hash__(self) -> int:
        return hash(f"{self.section}:{self.key}")

    def __call__(self) -> Any: pass

@runtime_checkable
class Config_i(Protocol):
    suffix      : ClassVar[str]    = ".config"
    root_logger : ClassVar[Logger] = getLogger('acab')

    @abc.abstractmethod
    def __call__(self, lookup) -> Any: pass
    @abc.abstractmethod
    def __contains__(self, key:str|Enum) -> bool: pass
    @abc.abstractmethod
    def check(self, spec: ConfigSpec_d) -> ConfigSpec_d: pass
    @abc.abstractmethod
    def default(self, entry:str) -> Any: pass
    @property
    @abc.abstractmethod
    def loaded(self) -> bool: pass
    @abc.abstractmethod
    def override(self, spec: ConfigSpec_d, value:str) -> None: pass
    @abc.abstractmethod
    def prepare(self, *args:Any, **kwargs:Any) -> ConfigSpec_d: pass
    @abc.abstractmethod
    def read(self, paths: list[str]) -> Config_i: pass
    @abc.abstractmethod
    def value(self, val: Enum | ConfigSpec_d) -> Any: pass
