from dataclasses import dataclass, field, InitVar
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from .acab_exception import AcabException

@dataclass
class AcabConfigException(AcabException):
    """ Exceptions relating to configuration"""

    msg : str = field(init=False, default="Configuration Failure")
