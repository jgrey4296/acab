from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

from .acab_exception import AcabException

class AcabParseException(AcabException):
    """ The base exception for parsing errors """
    msg : str = field(init=False, default="Parse Failure: {}")

    pass
