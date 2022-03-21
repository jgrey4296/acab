"""
The Core Value_A Classes: AcabValue, Instruction_A, Sentence
"""
import logging as root_logger
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
import acab.core.data.sub_implementations.value as VP
import acab.interfaces.value as VI
from acab import setup
from acab import types as AT
from acab.core.config.config import GET, AcabConfig
from acab.core.data.util import name_sieve_fns
from acab.core.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.error.protocol import AcabProtocolError as APE
from acab.interfaces.sieve import AcabSieve
from acab.core.data.factory import ValueFactory

logging          = root_logger.getLogger(__name__)

config           = AcabConfig()
BIND_SYMBOL      = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL   = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP        = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T           = TypeVar('T', bound=AT.ValueCore, covariant=True)

Value_A       : TypeAlias = AT.Value
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
ValueData     : TypeAlias = AT.ValueData


@APE.assert_implements(VI.Value_i)
@dataclass(frozen=True, repr=False, eq=False)
class AcabValue(VP.ValueProtocolsImpl, VI.Value_i):

    name_sieve : ClassVar[AcabSieve[str]] = AcabSieve(name_sieve_fns)

    @classmethod
    def build(cls, value:T, /, *,
              name:None|str=None,
              data:None|dict[ValueData, Any]=None,
              params:None|list['Value_A|str']=None,
              tags:None|list['Value_A|str']=None,
              _type:'None|str|Sen_A'=None,
              **kwargs) -> Value_A:
        """
        Idempotent construction.
        Wrap the provided value in an AcabValue,
        but only if it isn't an AcabValue already
        """
        _data        = ValueFactory._build_data_and_type(data, _type)
        tags, params = ValueFactory._build_tags_and_params(tags, params)

        if isinstance(value, VI.Value_i):
            return ValueFactory._handle_nesting(value, name, data)

        name         = AcabValue.name_sieve.fifo_first({'name': name, 'class':cls.__class__, 'value': value})
        new_obj = cls(value, name=name, data=_data, tags=tags, params=params, **kwargs)
        # Pre-initialisation, the ValueFactory value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return cast(VI.Value_i, new_obj)
