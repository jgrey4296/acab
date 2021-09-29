"""
Utility decorators
"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import logging as root_logger
from enum import Enum
logging = root_logger.getLogger(__name__)

from acab import types as AT

Structure = AT.DataStructure
Sentence  = AT.Sentence

def ForceListArgDecorator(f):
    """ Force the first arg to be a list """
    def wrapped(self, *the_args, **the_kwargs):
        forced = []
        if isinstance(the_args[0], list):
            forced = the_args
        else:
            forced.append([the_args[0]])
            forced += the_args[1:]

        return f(self, *forced, **the_kwargs)

    wrapped.__name__ = f"FLA({f})"
    return wrapped

def registerOn(cls:type):
    """ Decorator for registering a function onto a class as a method """

    def wrapper(fn):
        logging.info(f"Method Registration: {cls.__name__} . {fn.__name__}")
        assert(fn.__name__ not in dir(cls))
        setattr(cls, fn.__name__, fn)
        return fn

    return wrapper


<<<<<<< HEAD:acab/core/decorators/util.py
def mapToEnum(the_dict:Dict[Enum, Any], enum_v:Enum):
    def wrapper(fn):
        the_dict[enum_v] = fn
        return fn

    return wrapper
=======
def cache(f):
    cache_key = f"{f.__name__}__cached_val"
    def wrapped(self, *args):
        if hasattr(self, cache_key):
            return getattr(self, cache_key)

        object.__setattr__(self, cache_key, f(self, *args))
        return getattr(self, cache_key)

    wrapped.__name__ = f"Cached({f.__name__})"
    return wrapped
>>>>>>> 57fd09ba ([decorator] add @cache decorator):acab/abstract/decorators/util.py
