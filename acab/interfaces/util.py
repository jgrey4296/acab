#!/usr/bin/env python3

import abc
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence,
                    Tuple, TypeAlias, TypeVar, cast)
from uuid import UUID, uuid1

from acab import types as AT


@dataclass(frozen=True)
class AcabUUID(metaclass=abc.ABCMeta):
    """
    Designate a class as having a unique UUID which
    is tracked through lineages
    """
    _uuid : UUID = field(init=False, default_factory=uuid1)

    @property
    def UUID(self) -> UUID:
        pass

    def __hash__(self):
        return hash(self.UUID)

class AcabReducible(metaclass=abc.ABCMeta):
    """ Designates a class as being able to reduce down
    to a list of sentences """

    @abc.abstractmethod
    def to_sentences(self) -> list[AT.Sentence]:
        """ Convert to sentences for printing """
        pass

    @abc.abstractmethod
    def from_sentences(self, sens:list[AT.Sentence]) -> list[Any]:
        pass

    @abc.abstractmethod
    def to_word(self) -> AT.Value:
        pass

class AcabFinishable(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def finish(self):
        """
        Idempotent transform for static/mutable structures (like ContextInstances),
        to allow lifting to static
        """
        pass


class AcabBuildable(metaclass=abc.ABCMeta):
    @staticmethod
    @abc.abstractmethod
    def build(*args, **kwargs) -> 'AcabBuildable':
        pass
