#!/usr/bin/env python3
"""
Provides Metaclasses for creating Singletons

Note: superclass is tyep(Protocol) so classes which
implement protocol's don't get a metaclass conflict
"""
import logging as root_logger
from fractions import Fraction
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence, Protocol,
                    Set, Tuple, TypeAlias, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

class SingletonMeta(type(Protocol)):
    """
    Create an instance field to hold the singleton
    For Each Class Hierarchy
    """
    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(SingletonMeta, cls).__init__(name, bases, data)
        if not hasattr(cls, "_instance"):
            cls._instance = None

    def __call__(cls, *args: Any) -> type:
        if cls._instance is None:
            cls._instance = super().__call__(*args)

        return cls._instance


class SingletonMetaAlt(type(Protocol)):
    """
    Create an instance field to hold the singleton,
    Subclasses are separate singletons
    """

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(SingletonMetaAlt, cls).__init__(name, bases, data)
        cls._instance = None

    def __call__(cls, *args:Any) -> type:
        if cls._instance is None:
            cls._instance = super().__call__(*args)

        return cls._instance

class PoolMeta(type(Protocol)):
    """
    Create an instance field to hold the singleton,
    Subclasses are separate singletons
    """
    _pool_size = 10

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(PoolMeta, cls).__init__(name, bases, data)
        cls._pool      : list[Any] = []
        cls._pool_last = 0

    def __call__(cls, *args:Any) -> type:
        if cls._pool_last > cls._pool_size - 1:
            cls._pool_last = 0

        if len(cls._pool) < cls._pool_size:
            obj = super().__call__(*args)
            cls._pool.append(obj)
        else:
            obj = cls._pool[cls._pool_last]
            obj.__init__(*args)

        cls._pool_last += 1
        return obj
