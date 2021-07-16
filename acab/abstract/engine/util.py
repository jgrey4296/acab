from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field


def usable_comp(val, base_type):
    return isinstance(val, base_type)

def applicable_comp(val, base_type):
    return isinstance(val, base_type) or isinstance(val, base_type) and issubclass(val, base_type)

def comp_needs_instantiation(val, base_type):
    return (not val == base_type) and isinstance(val, type) and issubclass(val, base_type)


@dataclass
class ModuleComponents():
    """ Simple holder for extracted module components """

    dsl_fragments : Any = field()
    semantics     : Any = field()
    operators     : Any = field()
    printers      : Any = field()
