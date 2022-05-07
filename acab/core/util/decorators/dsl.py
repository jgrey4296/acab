from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, Protocol,
                    Sequence, Tuple, TypeAlias, TypeGuard, TypeVar, cast)

if TYPE_CHECKING:
    # tc only imports
    pass

from functools import wraps

from acab.error.parse import AcabParseException

T = TypeVar('T')

#pylint: disable-next=invalid-name
def EnsureDSLInitialised(method:Callable[..., T]) -> Callable[..., T]:
    """ Utility Decorator for DSL Builder's, raising error if not initialised """
    #pylint: disable-next=invalid-name
    @wraps(method)
    def dsl_must_be_initialised(self, *args, **kwargs):
        if not self._parsers_initialised:
            raise AcabParseException("DSL Not Initialised")

        return method(self, *args, **kwargs)

    return dsl_must_be_initialised
