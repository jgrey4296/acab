#/usr/bin/env python3
"""
A Module of utility sentence constructors for building standardized patterns.

eg: A ProductionComponent has the form:
[[operator path].[ parameter sentences ].[returns.$variable]]
of type SENTENCE.component

"""
##-- imports
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

import acab
from acab.core.defaults import value_keys as DS
from acab.interfaces.value import ValueFactory as VF

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

##-- end imports

config = acab.config

prod_comp_type = VF.sen() << DS.SENTENCE_PRIM << DS.COMPONENT_PRIM

def ProductionComponent(path, name=None, params=None, rebind="unit", _type=None):
    return (VF.sen(data={DS.TYPE_INSTANCE: prod_comp_type << _type})
            << path << (VF.sen() << (params or "∅"))
            << (VF.sen() << "returns" << rebind))
