"""
The Core Value_A Classes: AcabValue, Instruction_A, Sentence
"""
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from functools import reduce
from re import Pattern
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, Type,
                    TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab.interfaces.data as DI
import acab.core.defaults.value_keys as DS
import acab.core.util.part_implementations.value as VP
import acab.interfaces.value as VI
from acab import AcabConfig
from acab import types as AT
from acab.core.util.decorators.util import cache
from acab.core.value.value_meta import ValueMeta
from acab.error.base import AcabBasicException
from acab.error.protocol import AcabProtocolError as APE
from acab.interfaces.value import ValueFactory

logging        = logmod.getLogger(__name__)

config         = AcabConfig()
BIND_SYMBOL    = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP      = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T              = TypeVar('T', bound=AT.ValueCore, covariant=True)

Value_A       : TypeAlias = AT.Value
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
ValueData     : TypeAlias = AT.ValueData


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
