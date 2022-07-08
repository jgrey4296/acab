#!/usr/bin/env python3
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

import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
from acab import types as AT
from acab import AcabConfig
from acab.core.util.decorators.util import cache
from acab.error.base import AcabBasicException
from acab.interfaces.sieve import AcabSieve
import acab.interfaces.protocols.value as VP
from acab.interfaces.value import ValueFactory
from acab.core.util.debugging import logdel

logging        = logmod.getLogger(__name__)

config         = AcabConfig()
BIND_SYMBOL    = config.prepare("Symbols", "BIND")()
FALLBACK_MODAL = config.prepare("Symbols", "FALLBACK_MODAL", actions=[config.actions_e.STRIPQUOTE])()

UUID_CHOP      = bool(int(config.prepare("Print.Data", "UUID_CHOP")()))

T              = TypeVar('T', bound=AT.ValueCore)

Value_A       : TypeAlias = AT.Value
Sen_A         : TypeAlias = AT.Sentence
Instruction_A : TypeAlias = AT.Instruction
ValueData     : TypeAlias = str

class CacheValueProtocolMods(VI.Value_i):
    """
    A Utility class for default implementations of Value_i methods
    """
    # TODO update
    def __eq__(self, other):
        """ Base eq: compare hashes  """
        if id(self) == id(other):
            return True
        elif isinstance(other, str):
            return str(self) == other
        elif isinstance(other, UUID):
            return self.uuid == other
        elif not isinstance(other, VI.Value_i):
            return False
        elif self.uuid == other.uuid:
            return True
        else:
            return str(self) == str(other)

    def __lt__(self, other):
        if isinstance(other, VI.Value_i):
            return self.name < other.name
        elif isinstance(other, str):
            return self.name < other

        return TypeError("AcabValues can only be __lt__'d with AcabValues and Strings")

    # TODO Cache this
    @property #type:ignore
    def type(self) -> Sen_A:
        """ Lazily coerces type description to Sentence """
        type_desc = self.data[DS.TYPE_INSTANCE]
        assert(type_desc is not None)
        type_matches_t = isinstance(type_desc, VI.Sentence_i)
        if type_matches_t:
            return type_desc # type:ignore

        assert(isinstance(type_desc, (DS.DATA_STRUCT_E, str)))
        if DS.SEMANTIC_HINT in self.data and isinstance(self.data[DS.SEMANTIC_HINT], VI.Sentence_i):
            self.data[DS.TYPE_INSTANCE] = self.data[DS.SEMANTIC_HINT]
        else:
            self.data[DS.TYPE_INSTANCE] = ValueFactory.sen([type_desc])

        return self.data[DS.TYPE_INSTANCE] # type:ignore
