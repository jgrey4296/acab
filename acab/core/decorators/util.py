"""
Utility decorators
"""
#pylint: disable=invalid-sequence-index
from __future__ import annotations
import logging as logmod
from enum import Enum
from functools import wraps
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, ParamSpec,
                    Sequence, Set, Tuple, TypeAlias, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from acab import types as AT

Structure : TypeAlias = 'AT.DataStructure[AT.Node]'
Sentence  : TypeAlias = AT.Sentence

T = TypeVar('T')
P = ParamSpec('P')

def ForceListArgDecorator(f:Callable[..., T]) -> Callable[..., T]:
    """ Force the first arg to be a list """
    @wraps(f)
    def wrapped(self, *the_args, **the_kwargs):
        forced = []
        if isinstance(the_args[0], list):
            forced = the_args
        else:
            forced.append([the_args[0]])
            forced += the_args[1:]

        return f(self, *forced, **the_kwargs)

    return wrapped

def registerOn(cls:type) -> Callable[..., Any]:
    """ Decorator for registering a function onto a class as a method """
    def wrapper(fn):
        logging.info(f"Method Registration: {cls.__name__} . {fn.__name__}")
        assert(fn.__name__ not in dir(cls))
        setattr(cls, fn.__name__, fn) #type:ignore
        return fn

    return wrapper


def mapToEnum(the_dict:dict[Enum, Any], enum_v:Enum) -> Callable[..., Any]:
    """
    Utility decorator to simplify creating a mapping of enum entries to
    functions.
    """
    def wrapper(fn):
        the_dict[enum_v] = fn
        return fn

    return wrapper

def cache(f:Callable[..., T]) -> Callable[..., T]:
    cache_key : str = f"{f.__name__}.__cached_val"
    def wrapped(self, *args, **kwargs):
        if hasattr(self, cache_key): #type:ignore
            return cast(T, getattr(self, cache_key)) #type:ignore

        object.__setattr__(self, cache_key, f(self, *args, **kwargs)) #type:ignore
        return getattr(self, cache_key) #type:ignore

    wrapped.__name__ = f"Cached({f.__name__})"
    return wrapped #type:ignore

def invalidate_cache(f:Callable[..., T]) -> Callable[..., T]:
    cache_key = f"{f.__name__}__cached_val"
    def wrapped(self, *args, **kwargs):
        if hasattr(self, cache_key): #type:ignore
            object.__setattr__(self, cache_key, None) #type:ignore

        return f(self, *args, **kwargs)

    wrapped.__name__ = f"Cached({f.__name__})"
    return wrapped

def singleton(orig_cls:Any) -> Any:
    """ From:
    https://igeorgiev.eu/python/design-patterns/python-singleton-pattern-decorator/
    DEPRECATED, use singleton metas instead
    """
    orig_new = orig_cls.__new__
    instance = None

    @wraps(orig_cls.__new__)
    def __new__(cls, *args, **kwargs):
        nonlocal instance
        if instance is None:
            instance = orig_new(cls, *args, **kwargs)
        return instance

    orig_cls.__new__ = __new__
    return orig_cls


def HandleSignal(sig : str) -> Callable[..., Any]:
    """
    Utility to easily add a signal to classes for use in the handler system
    """
    def wrapper(cls):
        cls.__init__ = __add_sig(sig, cls.__init__) #type:ignore
        return cls

    return wrapper

def __add_sig(sig, method):
    """ used to add 'signal' as an argument to a class' init method """
    def wrapper(self, *args, **kwargs):
        if 'signal' not in kwargs:
            kwargs['signal'] = sig #type:ignore
        return method(self, *args, **kwargs)

    return wrapper


def factory(f:Callable[..., T]) -> Callable[..., T]:
    """
    Curry a constructor
    """
    @wraps(f)
    def awaiting_arg(first_arg:Any) -> Callable[..., T]:
        @wraps(f)
        def ready_to_apply(*args:Any, **kwargs:Any) -> T:
            return f(first_arg, *args, **kwargs)

        ready_to_apply.__name__ = f"{f.__name__} partial"
        return ready_to_apply

    return awaiting_arg #type:ignore
