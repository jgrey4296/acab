"""

"""
##-- import
from __future__ import annotations

import abc
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    # tc only imports
    pass

from acab import types as AT
import acab
##-- end import

config = acab.config

cmd_fn : TypeAlias = Callable[[Cmd, str], ]

class RegisterableCmd_p(Protocol):
    def _gen_parser(self) -> Callable[[str], Any]: pass
    def __call__(self, line: str) -> None: pass
