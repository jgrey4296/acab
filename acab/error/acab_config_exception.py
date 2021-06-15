from dataclasses import dataclass, field, InitVar
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from .acab_base_exception import AcabBaseException

@dataclass
class AcabConfigException(AcabBaseException):
    """ Exceptions relating to configuration"""

    msg : str = field(init=False, default="Configuration Failure: {}")

    def __str__(self):
        return self.msg.format(self.detail)
