from __future__ import annotations
import re
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import AcabConfig
from acab.interfaces.handler_system import Handler_i

config = AcabConfig()

MODULE_SPLIT_REG = re.compile(config.attr.Parse.Patterns.MODULE_SPLIT_REG)

def applicable(val:Any, base_type:type, *, as_handler=False) -> bool:
    """
    Test whether an input is of an expected type instance, including subclasses,
    but *not* the type itself.
    Useful for selecting implementations of an ABC

    Can also select for handlers containing the correct type as well.
    """
    if as_handler and isinstance(val, Handler_i):
        val = val.func #type:ignore

    not_base    = val is not base_type
    if isinstance(base_type, tuple):
        not_base    = all([val is not x for x in base_type])

    is_type     = isinstance(val, type)
    is_subclass = is_type and issubclass(val, base_type)
    is_instance = not is_type and isinstance(val, base_type)

    return not_base and (is_subclass or is_instance)

def needs_init(val) -> bool:
    """
    Test for whether an input is of type 'type',
    thus needs to be instantiated to become a usable value
    """
    return isinstance(val, type)

def ensure_handler(val) -> Handler_i:
    """
    Ensure a value is initialised and wrapped in a handler
    """
    if isinstance(val, Handler_i):
        return val
    if needs_init(val):
        val = val()

    return val.as_handler()


def prep_op_path(package:str, operator_name:str) -> list[str]:
    """
    Canonical conversion of module paths to words for full operator location sentences
    """
    words = MODULE_SPLIT_REG.split(package)
    words += [operator_name]
    return words
