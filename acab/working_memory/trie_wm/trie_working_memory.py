""" The Core Trie Data Structure base """
import logging as root_logger
from copy import deepcopy

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.abstract.config.config import AcabConfig

from acab.abstract.core.contexts import Contexts
from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.interfaces.working_memory_interface import WorkingMemoryCore
from acab.abstract.containers.production_abstractions import ProductionContainer
from acab.error.acab_operator_exception import AcabOperatorException
from acab.modules.node_semantics import exclusion_semantics as ES
from acab.modules.structures.trie.trie import Trie
from acab.modules.structures.trie.trie_semantics import BasicTrieSemantics

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

NEGATION_S       = config.value("Parse.Structure", "NEGATION")
QUERY_FALLBACK_S = config.value("Parse.Structure", "QUERY_FALLBACK")

class TrieWM(WorkingMemoryCore):
    """ A Trie based working memory"""

    def __init__(self, init: List[str]=None):
        """ init is a string of assertions to start the fact base with """
        # TODO enable passing of starting node semantics
        super().__init__()
        semantics = BasicTrieSemantics({AcabNode : ES.ExclusionNodeSemantics()},
                                       {AcabValue : (AcabNode, {})})
        self._structure        = Trie(semantics)
        if init is not None:
            self.add(init)

    def __str__(self):
        return str(self._structure)

    def __eq__(self, other):
        if isinstance(other, TrieWM):
            return self._structure._root == other._structure._root
        elif isinstance(other, Trie):
            return self._structure._root == other._root
        else:
            raise AcabOperatorException("Incorrect Eq arg: {}".format(type(other)))


    def add(self, data: List[Sentence], leaf=None, semantics=None):
        """ Assert multiple facts from a single string """
        use_semantics = semantics or self._structure.semantics

        if len(assertions) == 1 and leaf:
            assertions = [assertions[0].attach_statement(leaf)]

        return use_semantics.add(self._structure, assertions)


    def query(self, query, ctxs=None, engine=None, semantics=None):
        """ Query a string, return a Contexts """
        use_semantics = semantics or self._structure.semantics
        if isinstance(query, Sentence):
            query = ProductionContainer([query])
        if not isinstance(query, ProductionContainer):
            raise AcabParseException("Unrecognised query target: {}".format(type(query)))

        return use_semantics.query(self._structure, query, ctxs=ctxs, engine=engine)



    def to_sentences(self, semantics=None):
        use_semantics = semantics or self._structure.semantics
        return use_semantics.down(self._structure)

