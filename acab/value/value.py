"""
The Core Value_A Classes: AcabValue, Instruction_A, Sentence
"""
##-- imports
from __future__ import annotations

import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from functools import reduce
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, Sequence, Tuple,
                    Type, TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab
import acab.core.value.part_implementations.value as VP
import acab.interfaces.data as DI
import acab.interfaces.value as VI
from acab.core.metaclasses.value import ValueMeta
from acab.core.util.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.value import ValueFactory
from acab_config import AcabProtocolError as APE

if TYPE_CHECKING:
    from acab import types as AT
    ValueData     : TypeAlias = AT.ValueData
    ValueCore = AT.ValueCore
else:
    ValueCore = "ValueTypes"

##-- end imports

Value_A       : TypeAlias = VI.Value_i
Sen_A         : TypeAlias = VI.Sentence_i
Instruction_A : TypeAlias = VI.Instruction_i
T                         = TypeVar('T', bound=ValueCore, covariant=True)

logging        = logmod.getLogger(__name__)
config         = acab.config
BIND_SYMBOL    = config.any_of().symbols.BIND()
FALLBACK_MODAL = config.any_of().symbols.FALLBACK_MODAL()
UUID_CHOP      = config.any_of().print.UUID_CHOP()

@APE.assert_implements(VI.Value_i)
@dataclass(frozen=True, repr=False, eq=False)
class AcabValue(VP.ValueProtocolsImpl, VI.Value_i, metaclass=ValueMeta):
    _defaults : ClassVar[dict[str, Any]] = {}

    @classmethod
    def _preprocess(cls, *args, **kwargs):
        if isinstance(args[0], DI.Node_i):
            return args[0].value
        return args[0]

    @classmethod
    def _handle_nesting(cls, value, name=None, data=None, params=None, tags=None, _type=None, **kwargs):
        return value

    def __handle_nesting(cls, value, name=None, data=None, params=None, tags=None, _type=None, **kwargs):
        logging.debug("Attempted to nest a value, copying: {}", value)
        new_data = value.data.copy()
        new_data.update(data or {})
        # name = "nested"
        name   = kwargs['name'] if 'name' in kwargs else value.name
        tags   = tags | value.tags
        params = params or value.params
        val    = value.copy(data=new_data, name=name, tags=tags, params=params)
        raise DeprecationWarning()
