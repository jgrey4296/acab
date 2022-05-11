"""
The Core Value_A Classes: AcabValue, Instruction_A, Sentence
"""
from __future__ import annotations
import logging as logmod
from dataclasses import InitVar, dataclass, field, replace
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence, Tuple, Type,
                    TypeAlias, TypeVar, cast)

import acab.interfaces.value as VI
from acab import AcabConfig
from acab.modules.values.string_cache_val.caching_meta import StringCacheValueMeta
from acab.modules.values.string_cache_val import protocol_impl as VP
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue

logging        = logmod.getLogger(__name__)

config         = AcabConfig()

class StringCacheValue(VP.CacheValueProtocolMods, AcabValue, VI.Value_i, metaclass=StringCacheValueMeta):
    pass

class StringCacheSentence(VP.CacheValueProtocolMods, Sentence, VI.Sentence_i, metaclass=StringCacheValueMeta):
    pass
