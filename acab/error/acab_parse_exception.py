from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

from .acab_base_exception import AcabBaseException

class AcabParseException(AcabBaseException):
    """ The base exception for parsing errors """
    pass
