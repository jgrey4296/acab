"""
Utility decorators
"""
from functools import wraps
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import logging as logmod
logging = logmod.getLogger(__name__)

from acab import types as AT

Structure = AT.Structure
Sentence  = AT.Sentence

def LogHelper(prefix, level=logmod.DEBUG):
    """
    Utility Decorator to log a functions return value at a set level
    """
    def wrapper(f):
        @wraps(f)
        def in_wrap(*args, **kwargs):
            result = f(*args, **kwargs)
            log_msg = f"{prefix}: {result}"
            logmod.getLogger(__name__).log(level, log_msg)
            return result

        return in_wrap

    return wrapper
