"""
Base Class for typing module values.
Adds utility methods
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from dataclasses import dataclass, field, InitVar

from acab.core.config.config import AcabConfig
from acab.core.value.value import AcabValue
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence


config = AcabConfig()

@dataclass(frozen=True)
class TypeStatement(Instruction):
    # TODO: change value to a config value

    value : list[Sentence] = field(default_factory=list)
    name  : str            = field(default="|∀σ|")
    _path : Sentence       = field(default=None)

    def __post_init__(self, *args, **kwargs):
        pass

    def to_sentences(self):
        return self.structure[:]

    @property
    def head(self):
        return self._path[-1]

    @property
    def vars(self):
        return self.params

    @property
    def structure(self):
        return self.value
