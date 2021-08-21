import re
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import GET

config = GET()

MODULE_SPLIT_REG = re.compile(config.prepare("Parse.Patterns", "MODULE_SPLIT_REG")())

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




def prep_op_path(package:str, operator_name:str) -> List[str]:
    """
    Canonical conversion of module paths to words for full operator location sentences
    """
    words = MODULE_SPLIT_REG.split(package)
    words += [operator_name]
    return words
