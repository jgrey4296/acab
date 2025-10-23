"""

"""
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from .base import AcabBasicException


@dataclass(repr=False)
class AcabParseException(AcabBasicException):
    """ The base exception for parsing errors """
    msg       : str = field(init=False, default="Parse Failure:")
    file_name : str = field(init="")
