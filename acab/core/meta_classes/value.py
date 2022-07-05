"""
Metaclass for constructing AcabValues.
"""
from __future__ import annotations

import abc
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Dict, Generic,
                    Iterable, Iterator, List, Mapping, Match, MutableMapping,
                    Optional, Protocol, Sequence, Set, Tuple, TypeAlias,
                    TypeVar, Union, cast)
from uuid import UUID, uuid1
from weakref import ref

import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.util.decorators.util import cache
from acab.core.meta_classes.singletons import SingletonMeta
from acab.core.value.util import name_sieve_fns
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve

if TYPE_CHECKING:
    ValueData     : TypeAlias = str

Value_A       : TypeAlias = VI.Value_i
Sen_A         : TypeAlias = VI.Sentence_i
Instruction_A : TypeAlias = VI.Instruction_i
T              = TypeVar('T', bound=AT.ValueCore)
C              = TypeVar('C', bound=type)

ProtocolMeta = type(Protocol)

logging        = logmod.getLogger(__name__)
config         = AcabConfig()
BIND_SYMBOL    = config.attr.Symbols.BIND
FALLBACK_MODAL = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()
UUID_CHOP      = config.prepare("Print.Data", "UUID_CHOP", _type=bool)()

class ValueMeta(ProtocolMeta):
    """ Utility Meta Class for building values """

    name_sieve    : ClassVar[AcabSieve[str]]           = AcabSieve(name_sieve_fns)
    default_data  : ClassVar[dict[str, Any]]           = {DS.TYPE_INSTANCE : DS.TYPE_BASE, DS.BIND : False}
    __subclasses  : ClassVar[dict[str,type[Value_A]]]  = dict()

    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(ValueMeta, cls).__init__(name, bases, data)

        full_name = f"{cls.__module__}.{cls.__qualname__}"
        if full_name in ValueMeta.__subclasses:
            raise TypeError("Duplicated Type: {}".format(full_name))
        # Keep track of all subclasses
        ValueMeta.__subclasses[full_name] = cls

    def __call__(cls, value:T=None, *, name=None, data=None, params=None, tags=None, _type=None, **kwargs) -> Value_A:
        """
        The Meta Constructor for Values, to set up standard elements prior to insertion into
        the immutable form
        """
        # TODO possibly hold weafrefs to all created objects, and avoid any duplication
        defaults     = getattr(cls, "_defaults", {})
        _data        = cls._build_data_and_type(data, _type, defaults=defaults)
        tags, params = cls._build_tags_and_params(tags, params)

        # eg: To handle lifting a sentence's list[str] to list[Value]
        if hasattr(cls, '_preprocess'):
            value = cls._preprocess(value)

        if not (isinstance(value, AT.ValueCore | VI.Value_i)):
            # TODO use an acab error here
            raise TypeError((value, type(value)))

        # If the value to be wrapped is already an AcabValue, follow separate nesting rules
        if isinstance(value, VI.Value_i) and hasattr(cls, '_handle_nesting'):
            return cls._handle_nesting(value, name, data, params, tags, _type, **kwargs)

        # Sieve for a name
        name    = ValueMeta.name_sieve.fifo_first({'name': name, 'class':cls, 'value': value})
        # Build the actual value
        new_obj = super(ValueMeta, cls).__call__(value, name=name, data=_data, params=params, tags=tags, **kwargs)

        return new_obj

    @staticmethod
    def _build_tags_and_params(tags:None|Iterable[Any], params:None|Iterable[Any]) -> tuple[list[VI.Value_i[str]], frozenset[VI.Value_i[str]]]:
        """ Standardized conversion of tags and params to values

        Enforces invariants:
        params are a list of vars
        tags are a frozenset of values
        """
        if params is None:
            params = []
        param_vals : list[VI.Value_i[str]] = list([VI.ValueFactory.value(x, data={DS.BIND:True}) for x in params])

        if tags is None:
            tags = []
        tag_vals : list[VI.Value_i[str]] = [VI.ValueFactory.value(x) for x in tags]
        tag_set : set[VI.Value_i[str]]   = frozenset(tag_vals)

        return tag_set, param_vals

    @staticmethod
    def _build_data_and_type(data:None|dict[ValueData, Any], _type:'None|str|Sen_A'=None, defaults:dict[ValueData, Any]=None) -> dict[str,Any]:
        # TODO construct defaults in AcabConfig
        _data = {}
        _data.update(ValueMeta.default_data)
        _data.update(defaults)
        _data.update(data or {})

        if _type is not None:
            _data.update({DS.TYPE_INSTANCE: _type})

        return _data
