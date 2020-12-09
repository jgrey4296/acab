#!/usr/bin/env python3
from typing import Callable, Iterator, Union, Match
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import abc


class FlattenInterface(metaclass=abc.ABCMeta):
    """ Can Flatten itself into a set of sentences """

    @property
    @abc.abstractmethod
    def to_sentences(self) -> List['Sentence']:
        pass
