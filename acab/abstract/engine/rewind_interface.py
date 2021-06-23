"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.interfaces.dsl_interface import DSL_Interface, Bootstrapper
from acab.abstract.parsing.TrieBootstrapper import TrieBootstrapper

ModuleType   = 'Module'
Parser       = 'Parser'
Sentence     = 'Sentence'
DSL_Fragment = 'DSL_Fragment'

@dataclass
class RewindEngineInterface(metaclass=abc.ABCMeta):
    """
    Describes how an engine can be reverted to a previous state
    """
    # TODO  update with reloadable state of working memory
    prior_states : List[List['Sentence']] = field(default_factory=list)
    # named recall states of past kb states
    recall_states : Dict[str, List['Sentence']]= field(default_factory=dict)

    def rewind(self, val:Optional[Union[int, str]]) -> None:
        raise NotImplementedError()

    def save_state(self, name: Optional[str]=None):
        """ Copy the current string representation of the working memory,
        and any associated data """
        # TODO replace this with a down
        self.prior_states.append(self._working_memory.to_sentences())
        if name is not None:
            self.recall_state[name] = self._working_memory.to_sentences()


