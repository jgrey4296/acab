from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

from .base import AcabBasicException

@dataclass(repr=False)
class AcabSemanticException(AcabBasicException):
    """ The Core exception report of semantic operations  """

    msg : str = field(init=False, default="Semantic Failure:\n\t\t{}\n\t\tat: {}")

    def __str__(self):
        return self.msg.format(self.detail, self.context)

class AcabOperatorMissingException(AcabSemanticException):
    """ Raised when an operator can't be found when running semantics """
    pass

class AcabSemanticTestFailure(AcabSemanticException):
    """ Raised by ConstraintCollection when a test fails,
    for ContextSet to handle
    """
    pass

class AcabSemanticQueryContextDepletionFailure(AcabSemanticException):
    """ Raised by Dependent Semantics to signal the current
    ContextInstance can't progress any further """
    pass

class AcabSemanticIndependentFailure(AcabSemanticException):
    """ Signals failures in the node semantics """
    pass
