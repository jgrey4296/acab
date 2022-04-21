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

import acab.core.data.default_structure as DS
import acab.core.util.part_implementations.value as VP
import acab.interfaces.value as VI
from acab import setup
from acab import types as AT
from acab.core.config.config import GET, AcabConfig
from acab.core.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.error.protocol import AcabProtocolError as APE
from acab.core.data.factory import ValueFactory
from acab.core.data.value_meta import ValueMeta

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
        return args[0]

    @classmethod
    def _handle_nesting(cls, value, name=None, data=None, params=None, tags=None, _type=None, **kwargs):
        logging.debug("Attempted to nest a value, copying: {}", value)
        new_data = value.data.copy()
        new_data.update(data or {})
        # name = "nested"
        name   = kwargs['name'] if 'name' in kwargs else value.name
        tags   = tags | value.tags
        params = params or value.params
        return value.copy(data=new_data, name=name, tags=tags, params=params)
