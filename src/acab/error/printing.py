"""

"""
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from .base import AcabBasicException


@dataclass(repr=False)
class AcabPrintException(AcabBasicException):

    msg : str = field(init=False, default="Print Failure: {}")

    def __str__(self):
        return self.msg.format(self.detail)
