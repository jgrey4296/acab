"""
Dataclass for wrapping annotations which are applied onto values

"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field, InitVar

from acab import types as AT
from acab.abstract.config.config import GET

config = GET()

Value = AT.Value

@dataclass
class ValueAnnotation:
    """
    Result of parsing, which will be applied onto an AcabValue's data dict

    ValueAnnotation(x) + AcabValue -> AcabValue'[x]
    """

    key   : str = field()
    value : Any = field(default=None)

    def __call__(self, val:Value) -> Value:
        """ Apply the annotation """
        val.data[self.key] = self.value
        return val

class ValueRepeatAnnotation(ValueAnnotation):
    """
    Version of ValueAnnotation which handles multiple separate annotations of the
    same key, packaging them into a list

    ValueRepeatAnnotation(x) + AcabValue -> AcabValue'[x]
    ValueRepeatAnnotation(y) + AcabValue'[x] -> AcabValue'[x, y]
    """

    def __call__(self, val:Value) -> Value:
        if self.key not in val.data:
            val.data[self.key] = []
        val.data[self.key].append(self.value)
        return val



class ModalAnnotation(ValueAnnotation):

    def __call__(self, val:Value) -> Value:
        modal_value = config.syntax_extension[self.key]
        val.data[modal_value.__class__.__name__] = modal_value
        return val
