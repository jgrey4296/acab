from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

@dataclass
class AcabException(Exception):
    """ The base exception class for the Acab package """

    def __repr__(self):
        return f"<{self.__class__.__name__}>"

@dataclass(repr=False)
class AcabBasicException(AcabException):
    detail  : str           = field()
    context : Optional[str] = field(default=None, kw_only=True)
    msg     : str           = field(default="Non-specific Acab Error Raised", kw_only=True)
    rest    : List[Any]     = field(default_factory=list, kw_only=True)

    def __str__(self):
        msg = f"{self.msg} : {self.detail}"
        if bool(self.rest):
            msg += ": " + " ".join(self.rest)

        return msg
