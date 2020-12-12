#!/usr/bin/env python3
"""
The abstract form of a working memory

Working Memory is responsible for the core system capabilities.
It provides an ontology structure, a base parser for that structure,
and integrates dsl fragments into that base.

The canonical working memory is the Trie_WM.
It uses an Exclusion Logic Semantics on a Trie data structure,
Parsers to define sentences and rules in that data structure,
and can load modules to extend its capabilities.

The working memory, at core, can Add, Retract, and Query facts.

From a module it loads Value, Statement, and Annotation parsers.

"""
import abc
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field

from fractions import Fraction

from acab.abstract.interfaces.flatten_interface import FlattenInterface
from acab.abstract.interfaces.dsl_interface import DSL_Interface

Structure       = 'AcabStructure'
Sentence        = 'Sentence'
BootstrapParser = 'BootstrapParser'
Parser          =  'PyParsing.ParserElement'



@dataclass
class WorkingMemoryInterface(FlattenInterface, metaclass=abc.ABCMeta):
    """ The Core Interface. Add/Retract and Query the WM """
    _structure : Structure = field(init=False)

    @property
    def to_sentences(self) -> List[Sentence]:
        """ A simple passthrough """
        return self._structure.to_sentences

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
    """ The core used to *debug* WM action, using listeners """
    _listeners : Set[Any] = field(init=False, default_factory=set)
    _listeners_threshold : Fraction = field(init=False, default=Fraction(1,2))

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
    """ Enables the assemblage of a parser from DSL Fragments """
    _bootstrap_parser    : BootstrapParser = field(init=False)
    _main_parser         : Parser          = field(init=False)
    _query_parser        : Parser          = field(init=False)
    _parsers_initialised : bool            = False

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

    def clear_bootstrap(self):
        pass



class WorkingMemoryCore(WorkingMemoryInterface,
                        InterruptableWMInterface,
                        DSLBuilderInterface):
    """ The Assembled Working Memory Interface waiting for implementation """
    pass
