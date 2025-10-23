"""

"""
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from .base import AcabBasicException


@dataclass(repr=False)
class AcabHandlerException(AcabBasicException):
    msg : str = field(init=False, default="Handler Failure")


@dataclass(repr=False)
class HandlerDuplicationException(AcabHandlerException):
    msg : str = field(init=False, default="Duplicated handler in HandlerSpec")
