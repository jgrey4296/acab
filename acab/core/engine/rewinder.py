"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT

ModuleType    = 'Module'
Parser        = 'Parser'
Sentence      = AT.Sentence
DSL_Fragment  = AT.DSL_Fragment
RewindElement = Tuple[list[Sentence], str]

@dataclass
class RewindEngineInterface:
    """
    Describes how an engine can be reverted to a previous state
    """
    # TODO  update with reloacable state of working memory
    prior_states : list[RewindElement]       = field(default_factory=list)
    # named recall states of past kb states
    recall_states : dict[str, RewindElement] = field(default_factory=dict)

    def rewind(self, val:None|int|str) -> None:
        """
        Trigger the engine to store current state,
        and reload the passed in prior state.
        """
        raise NotImplementedError()

    def save_state(self, name: None|str=None):
        """ Copy the current string representation of the working memory,
        and any associated data """
        # TODO replace this with a down
        self.prior_states.append(self._working_memory.to_sentences())
        if name is not None:
            self.recall_state[name] = self._working_memory.to_sentences()


