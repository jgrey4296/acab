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
