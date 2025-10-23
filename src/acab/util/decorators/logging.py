"""
Utility decorators
"""
##-- imports
from __future__ import annotations

import logging as logmod
from functools import wraps
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    # tc only imports
    pass

##-- end imports

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
