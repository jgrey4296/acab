#!/usr/bin/env python3
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from acab.abstract.data.node import AcabNode
from acab.abstract.core.core_abstractions import AcabValue
from acab.abstract.data.structure import DataStructure
from acab.abstract.core.core_abstractions import Sentence
from acab.abstract.data.contexts import Contexts
from acab.abstract.data.node_semantics import AcabNodeSemantics

from acab.abstract.data.struct_semantics import AcabStructureSemantics

class TypingStructSemantics(AcabStructureSemantics):
    # TODO Locate listeners in semantics not WM

    def __init__(self, node_semantics : AcabNodeSemantics, node_type=AcabNode):
        self._ns = node_semantics
        self._node_type = node_type

    def set_node_type(self, node_type : AcabNode):
        self._node_type = node_type

    def set_node_semantics(self, ns : AcabNodeSemantics):
        self._ns = ns


    def add(self, structure : DataStructure, to_add : List[Sentence]) -> List[AcabNode]:
        """ Inserting a coherent set of sentences into the structure """
        raise NotImplementedError()

    def get(self, structure : DataStructure, sentence) -> List[AcabNode]:
        """ Getting a path of nodes corresponding to the sentence """
        raise NotImplementedError()

    def contain(self, structure, sentence) -> bool:
        """ Can the sentence be found in the structure """
        raise NotImplementedError()

    def delete(self, structure, sentence) -> List[AcabNode]:
        """ Remove a sentence from the structure """
        raise NotImplementedError()


    def query(self, structure, clause : Sentence, ctxs : Contexts, engine : 'Engine'):
        """ Answer a clause asked of the data structure """
        # TODO is this part of call semantics?
        # open / closed world
        # depth / breath search
        # match as pattern?
        # return type
        raise NotImplementedError()


    def down(self, data_structure : DataStructure) -> List[Sentence]:
        """ Take a complex data structure down to basic sentences """
        raise NotImplementedError()

    def lift(self, sentences : List[Sentence]) -> DataStructure:
        """ Raise a set of sentences into a data structure """
        raise NotImplementedError()

    def filter_candidates(self, structure, candidates, match_func):
        raise NotImplementedError()
