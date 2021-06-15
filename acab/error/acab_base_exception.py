from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

@dataclass
class AcabBaseException(Exception):
    """ The base exception class for the Acab package """

    msg   : str = field(default="Non-specific Acab Error Raised")
    rest  : List[Any] = field(default_factory=list)

    def __str__(self):
        if bool(self.rest):
            return f"{self.descr} : {self.rest}"

        return self.descr
