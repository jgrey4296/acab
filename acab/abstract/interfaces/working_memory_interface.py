#!/usr/bin/env python3
#!/usr/bin/env python3
import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass

class WorkingMemoryInterface(metaclass=abc.ABCMeta):
    """ """

    @abc.abstractmethod
    def to_sentences(self) -> List['Sentence']:
        pass

    @abc.abstractmethod
    def __str__(self):
        """ Print the working memory as a reparseable string """
        pass

    @abc.abstractmethod
    def __eq__(self, other):
        pass

    @abc.abstractmethod
    def add(self, data, leaf=None):
        """
        Add a sentence to the working memory.
        If leaf is specified, it is an AcabValue which will be attached
        as the leaf, with the sentence's name.
        eg: add(a.test.sentence.x, leaf=some_value) -> a.test.sentence.x(::some_value)
        """
        pass

    @abc.abstractmethod
    def query(self, ctxs=None, engine=None):
        pass





@dataclass
class InterruptableWMInterface(metaclass=abc.ABCMeta):
    _listeners : Set['Any']
    _listeners_threshold : 'Fraction'

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


@dataclass
class DSLBuilderInterface(metaclass=abc.ABCMeta):
    """ """
    _bootstrap_parser : 'BootstrapParser'

    def construct_parsers_from_fragments(self, fragments):
        """ Assemble parsers from the fragments of the wm and loaded modules """
        assert(all([isinstance(x, DSL_Interface) for x in fragments]))
        self.assert_parsers(self._bootstrap_parser)
        for x in fragments:
            #Populate the trie
            x.assert_parsers(self._bootstrap_parser)

        for x in fragments:
            # Now query and populate the modules
            x.query_parsers(self._bootstrap_parser)

        self.query_parsers(self._bootstrap_parser)

        for x in fragments:
            x.verify()

    def clear_bootstrap(self):
        self._bootstrap_parser = BootstrapParser()
