#!/usr/bin/env python3
# pylint: disable=multiple-statements,abstract-method,too-many-ancestors,invalid-sequence-index
from __future__ import annotations
import abc
import collections.abc as cABC
import logging as logmod
from dataclasses import dataclass, field
from functools import reduce
from re import Pattern
from typing import (Any, ClassVar, Container, Final, Generic, Literal, Mapping,
                    Match, MutableMapping, Protocol, Sequence, Sized, Tuple, Collection,
                    Type, TypeAlias, TypeVar, cast, runtime_checkable)
from uuid import UUID, uuid1

from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.error.config import AcabConfigException
import acab.interfaces.protocols.value as VSubP
import acab.core.value.default_structure as DS

logging       = logmod.getLogger(__name__)

@runtime_checkable
class Bind_i(Protocol):

    def __init__(self):
        raise TypeError("Bind is a static class, don't instantiate it, just call the static method `bind` ")

    @staticmethod
    @abc.abstractmethod
    def bind(val, bindings, semSys=None): pass
