"""

"""
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from .base import AcabBasicException


@dataclass(repr=False)
class AcabConfigException(AcabBasicException):
    """ Exceptions relating to configuration"""

    msg : str = field(init=False, default="Configuration Failure")
