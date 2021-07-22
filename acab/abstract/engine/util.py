from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field


def usable(val, base_type):
    return isinstance(val, base_type)

def applicable(val, base_type):
    not_base    = val is not base_type
    if isinstance(base_type, tuple):
        not_base    = all([val is not x for x in base_type])

    is_type     = isinstance(val, type)
    is_subclass = is_type and issubclass(val, base_type)
    is_instance = not is_type and isinstance(val, base_type)

    return not_base and (is_subclass or is_instance)

def needs_init(val, base_types=None):
    return isinstance(val, type)


