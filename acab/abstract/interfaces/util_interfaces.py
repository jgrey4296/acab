#!/usr/bin/env python3
from typing import Callable, Iterator, Union, Match
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import abc
from dataclasses import dataclass, field


class FlattenInterface(metaclass=abc.ABCMeta):
    """ Can Flatten itself into a set of sentences """

    @property
    @abc.abstractmethod
    def to_sentences(self) -> List['Sentence']:
        pass


@dataclass
class ProxyInterface(metaclass=abc.ABCMeta):

    def ask(self, lookup, for_uuid=None) -> Any:
        pass

    def use(self, lookup, for_uuid):
        pass



@dataclass
class InterruptableInterface(metaclass=abc.ABCMeta):
    """ The core used to *debug* WM action, using listeners """
    _listeners : Set[Any] = field(init=False, default_factory=set)
    _listeners_threshold : Fraction = field(init=False, default=Fraction(1,2))
    # TODO use these to enable breakpoint context:
    _production_stack : List['ProductionAbstraction'] = field(init=False, default_factory=list)

    def clear_listeners(self):
        self._listeners = set()

    def register_listeners(self, words):
        self._listeners.update(words)

    def unregister_listeners(self, words):
        self._listeners.difference_update(words)

    def set_listener_threshold(self, a, b):
        self._listener_threshold = Fraction(a,b)

    def score_listener(self, words):
        simple_words = [str(x) if not x.is_var else "$_" for x in words]
        num_in_listener_bag = sum([1 if x in self._listeners else 0 for x in simple_words])
        sentence_fraction = Fraction(num_in_listener_bag, len(simple_words))
        if sentence_fraction >= self._listener_threshold:
            return True

        return False

    def breakpoint(self):
        # TODO: add more listener options: pre, on and post
        breakpoint()




    def add_listeners(self, *words):
        """ Add basic data breakpoints """
        self._working_memory.register_listeners(words)

    def remove_listeners(self, *words):
        """ Remove data breakpoints """
        self._working_memory.unregister_listeners(words)

    def set_listener_threshold(self, a, b):
        """ Specify the number of word matches
        are needed to trigger the breakpoint """
        self._working_memory.set_listener_threshold(a, b)

    def get_listeners(self):
        return self._working_memory._listeners

    def get_listener_threshold(self):
        return self._working_memory._listener_threshold
