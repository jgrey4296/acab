"""
Dataclass for wrapping annotations which are applied onto values

"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

Value = "AcabValue"

@dataclass
class ValueAnnotation:
    """
    Result of parsing, which will be applied onto an AcabValue's data dict
    """

    key   : str = field()
    value : Any = field()

    def __call__(self, val:Value) -> Value:
        """ Apply the annotation """
        val.data[self.key] = self.value
        return val

class ValueRepeatAnnotation(ValueAnnotation):
    """
    Version of ValueAnnotation which handles multiple separate annotations of the
    same key, packaging them into a list
    """

    def __call__(self, val:Value) -> Value:
        if self.key not in val.data:
            val.data[self.key] = []
        val.data[self.key].append(self.value)
        return val
