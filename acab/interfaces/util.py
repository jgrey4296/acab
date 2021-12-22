#!/usr/bin/env python3

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import abc
from acab import types as AT


class AcabReducible(metaclass=abc.ABCMeta):
    """ Designates a class as being able to reduce down
    to a list of sentences """

    @abc.abstractmethod
    def to_sentences(self) -> List[AT.Sentence]:
        """ Convert to sentences for printing """
        pass

    #TODO add from_sentences
