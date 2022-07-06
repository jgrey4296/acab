#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from dataclasses import InitVar, dataclass, field, replace
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Protocol,
                    Sequence, Set, Tuple, TypeAlias, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.metaclasses.value import ValueMeta

logging        = logmod.getLogger(__name__)
config         = AcabConfig()

class SCAccessor:
    """
    A Targeted Accessor class, which caches it's target
    in StringCacheValueMeta
    """
    __slots__ = ["target"]

    def __init__(self, target):
        self.target = "_" + target

    def __get__(self, obj, objtype=None):
        return StringCacheValueMeta.uncache(getattr(obj, self.target))

    def __set__(self, obj, val):
        val = StringCacheValueMeta.maybe_cache(val)
        object.__setattr__(obj, self.target, val)

class StringCacheValueMeta(ValueMeta):
    """ Utility Meta Class for building values
    this form caches strings and just stores the hash value in the value
    TODO export this as a string table
    """

    cache_targets   : ClassVar[set[str]]              = set(["name", "value"])
    cache_accessors : ClassVar[dict[str, SCAccessor]] = {}
    _str_dict       : ClassVar[dict[int, str]]        = dict()

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        # Ensure values can handle ints
        VI.Value_i.extend_core(int)
        for target in StringCacheValueMeta.cache_targets:
            if target not in StringCacheValueMeta.cache_accessors:
                StringCacheValueMeta.cache_accessors[target] = SCAccessor(target)

            setattr(cls, target, StringCacheValueMeta.cache_accessors[target])

        super(ValueMeta, cls).__init__(name, bases, data)

    @staticmethod
    def size():
        """ Get the size of the cache
        (can't use len, because you it can't be a staticmethod)
        """
        return len(StringCacheValueMeta._str_dict)

    @classmethod
    def uncache(cls, val:int) -> str:
        """
        Get the cached value of an int
        """
        if isinstance(val, int):
            return cls._str_dict[val]

        return val

    @classmethod
    def maybe_cache(cls, val:str) -> int:
        """ Cache a value and return its hash """
        if not isinstance(val, str):
            return val

        hash_val = hash(val)
        if hash_val not in cls._str_dict:
            cls._str_dict[hash_val] = val

        return hash_val
