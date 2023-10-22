"""

"""
##-- imports
# pylint: disable=multiple-statements,abstract-method
from __future__ import annotations

from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    ParamSpec, Protocol, Sequence, Tuple, TypeAlias, TypeGuard,
                    TypeVar, cast, final, overload, runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

##-- end imports

# pylint: disable-next=invalid-name
T = TypeVar('T')
P = ParamSpec('P')
DecoratorVar  = TypeVar('DecoratorVar', bound=Callable[..., Any])

GenFunc       : TypeAlias = Callable[..., Any]
TypeFunc      : TypeAlias = Callable[..., type[Any]]
TestFunc      : TypeAlias = Callable[..., bool]
Parser        : TypeAlias = Callable[..., Any]
#DecoratorSpec : TypeAlias = Callable[P, T]
Sieve         : TypeAlias = Callable[..., T]
