#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

from acab import AcabConfig

config = AcabConfig()

from acab.core.defaults import value_keys as DS
from acab.interfaces.value import ValueFactory as VF

prod_comp_type = VF.sen() << DS.SENTENCE_PRIM << DS.COMPONENT_PRIM


def ProductionComponent(path, name=None, params=None, rebind=None, _type=None):
    return (VF.sen(data={DS.TYPE_INSTANCE: prod_comp_type << _type})
            << path << (VF.sen() << params)
            << "returns" << rebind)
