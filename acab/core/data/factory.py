#!/usr/bin/env python3
import abc
import logging as root_logger
from copy import deepcopy
from dataclasses import InitVar, dataclass, field, replace
from fractions import Fraction
from os import listdir
from os.path import (abspath, exists, expanduser, isdir, isfile, join, split,
                     splitext)
from re import Pattern
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeAlias, TypeVar, Union, cast)
from uuid import UUID, uuid1
from weakref import ref

logging = root_logger.getLogger(__name__)

import acab.core.data.default_structure as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve
from acab.core.util.singletons import SingletonMeta

logging        = root_logger.getLogger(__name__)

config         = AcabConfig()
BIND_SYMBOL    = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP      = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T              = TypeVar('T', bound=AT.ValueCore)
C              = TypeVar('C', bound=type)

Value_A       : TypeAlias = AT.Value
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
ValueData     : TypeAlias = str

class ValueFactory(VI.ValueFactory_i, metaclass=SingletonMeta):
    """ Utility Class for building values """
    @staticmethod
    def build(*args, **kwargs) -> C:
        """
        Idempotent construction.
        Wrap the provided value in an AcabValue,
        but only if it isn't an AcabValue already
        """
        new_obj = ValueFactory.value(*args, **kwargs)
        # Pre-initialisation, the ValueFactory value/sen _fns
        # just return an exception
        if isinstance(new_obj, Exception):
            raise new_obj
        return cast(VI.Value_i, new_obj)

    @staticmethod
    def _build_tags_and_params(tags:None|list[Any], params:None|list[Any]) -> tuple[frozenset[VI.Value_i], frozenset[VI.Value_i]]:
        if params is None:
            params = []
        params = list([ValueFactory.value(x, data={DS.BIND:True}) for x in params])

        if tags is None:
            tags = []
        tags = frozenset([ValueFactory.value(x) for x in tags])

        return tags, params

    @staticmethod
    def _build_data_and_type(data:None|dict[ValueData, Any], _type:'None|str|Sen_A'=None, defaults:None|dict[ValueData, Any]=None):
        # TODO construct defaults in config
        _data = {}
        _data.update(defaults or {})
        _data.update(data or {})

        if _type is not None:
            _data.update({DS.TYPE_INSTANCE: _type})
        elif DS.TYPE_INSTANCE not in _data:
            _data[DS.TYPE_INSTANCE] = DS.TYPE_BOTTOM_NAME

        if DS.BIND not in _data:
            _data[DS.BIND] = False

        return _data
    @staticmethod
    def _handle_nesting(value:T, name:None|str, data:None|dict[ValueData, Any]) -> T:
        logging.debug("Attempted to nest a value, copying")
        new_data = {}
        new_data.update(value.data)
        new_data.update(data or {})
        name = name or value.name
        return value.copy(data=new_data, name=name)
