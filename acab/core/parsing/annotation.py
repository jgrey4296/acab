"""
Dataclasses for wrapping annotations which are applied onto AcabValues

"""
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Dict, Generic,
                    Iterable, Iterator, List, Mapping, Match, MutableMapping,
                    Optional, Sequence, Set, Tuple, TypeVar, Union, cast)

from acab import AcabConfig
from acab.interfaces.value import Value_i

if TYPE_CHECKING:
    # tc only imports
    from acab import types as AT
    ValueData = AT.ValueData
else:
    ValueData = str

config = AcabConfig()

@dataclass
class ValueAnnotation:
    """
    Result of parsing, which will be applied onto an AcabValue's data dict

    ValueAnnotation(x) + AcabValue -> AcabValue'[x]
    """

    key   : ValueData = field()
    value : Any = field(default=None)

    def __call__(self, val:Value_i) -> Value_i:
        """ Apply the annotation """
        assert(isinstance(val,Value_i))
        val.data[self.key] = self.value
        return val

class ValueRepeatAnnotation(ValueAnnotation):
    """
    Version of ValueAnnotation which handles multiple separate annotations of the
    same key, packaging them into a list

    ValueRepeatAnnotation(x) + AcabValue -> AcabValue'[x]
    ValueRepeatAnnotation(y) + AcabValue'[x] -> AcabValue'[x, y]
    """

    def __call__(self, val:Value_i) -> Value_i:
        assert(isinstance(val,Value_i))
        if self.key not in val.data:
            val.data[self.key] = []
        val.data[self.key].append(self.value)
        return val



class ModalAnnotation(ValueAnnotation):

    def __call__(self, val:Value_i) -> Value_i:
        assert(isinstance(val,Value_i))
        modal_value = config.syntax_extension[self.key]
        val.data[modal_value.__class__.__name__] = modal_value
        return val
