"""
Base Class for typing module values.
Adds utility methods
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from dataclasses import dataclass, field, InitVar

from acab.core.config.config import AcabConfig
from acab.core.data.value import Instruction, Sentence

config = AcabConfig.Get()

@dataclass(frozen=True)
class TypeStatement(Instruction):
    # TODO: change value to a config value

    value : List[Sentence] = field(default_factory=list)
    name  : str            = field(default="|∀σ|")
    _path : Sentence       = field(default=None)

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
