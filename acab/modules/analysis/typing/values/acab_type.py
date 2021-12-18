"""
Base Class for typing module values.
Adds utility methods
"""
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.core.config.config import AcabConfig
from acab.core.data.value import Instruction

config = AcabConfig.Get()

class TypeStatement(Instruction):
    # TODO: change value to a config value

    def __init__(self, value="|∀σ|", **kwargs):
        super().__init__(value, **kwargs)
        self._path : 'Sentence'            = None
        self._structure : List['Sentence'] = []


    @property
    def head(self):
        return self.path[-1]

    @property
    def vars(self):
        return self.params

    @property
    def structure(self):
        return self._structure
