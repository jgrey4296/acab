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

from acab.abstract.interfaces.dsl_interface import DSL_Interface

Structure       = 'AcabStructure'
Sentence        = 'Sentence'
BootstrapParser = 'BootstrapParser'
Parser          =  'PyParsing.ParserElement'



@dataclass
class WorkingMemoryInterface(metaclass=abc.ABCMeta):
    """ The Core Interface. Add/Retract and Query the WM """
    _structure : Structure = field(init=False)

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





