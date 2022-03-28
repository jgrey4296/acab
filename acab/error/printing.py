from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

from .base import AcabBasicException

@dataclass(repr=False)
class AcabPrintException(AcabBasicException):

    msg : str = field(init=False, default="Print Failure: {}")

    def __str__(self):
        return self.msg.format(self.detail)
