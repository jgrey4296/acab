# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.contexts import Contexts
from acab.abstract.data.structure import DataStructure
from acab.abstract.data.node_semantics import AcabNodeSemantics

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.config import AcabConfig

util = AcabConfig.Get()

NEGATION_S = util("Parsing.Structure", "NEGATION_S")
CONSTRAINT_S = util("Parsing.Structure", "CONSTRAINT_S")
AT_BIND_S = util("Parsing.Structure", "AT_BIND_S")
ROOT_S = util("Data.Struct", "ROOT_S")

class AcabStructureSemantics:
    # TODO Locate listeners in semantics not WM

    def __init__(self, node_semantics : Dict[AcabNode, AcabNodeSemantics],
                 value_pairings: Dict[AcabValue, Tuple[AcabNode, Dict[Any, Any], Callable]]):
        self._node_semantics = node_semantics
        self._value_pairings = value_pairings
        # Todo: verify value -> node_c -> semantic chains

    def make_root(self):
        """
        Create a basic root node / entry point for a data structure
        """
        constructor, u_data, u_func = self._value_pairings[AcabValue]
        return constructor(ROOT_S)

    def semantics_for(self, node):
        assert(isinstance(node, type))
        # TODO should I be using my type instances for semantics?
        curr = node
        retrieved = None
        descendents_to_update = []
        while retrieved is None and curr not in (object, None):
            if curr in self._node_semantics:
                retrieved = self._node_semantics[curr]
            else:
                curr = curr.__base__
                descendents_to_update.append(curr)

        if retrieved is None:
            raise AcabSemanticException("Missing Node Semantic Binding for: {}".format(node),
                                        None)

        if len(descendents_to_update) > 1:
            self._node_semantics.update({x : retrieved for x in descendents_to_update})

        return retrieved

    def value_constructor(self, value):
        assert(isinstance(value, type))
        # TODO should I be using my type instances for semantics?
        curr = value
        retrieved = None
        descendents_to_update = []
        while retrieved is None and curr not in (object, None):
            if curr in self._value_pairings:
                retrieved = self._value_pairings[curr]
            else:
                curr = curr.__base__
                descendents_to_update.append(curr)


        if retrieved is None:
            raise AcabSemanticException("Missing Construction data for: {}".format(value),
                                        None)

        if len(descendents_to_update) > 1:
            self._value_pairings.update({x : retrieved for x in descendents_to_update})

        return retrieved


    def add(self, structure : DataStructure, to_add : List[Sentence], **kwargs) -> List[AcabNode]:
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

    def set_node_type(self, node_type : AcabNode):
        raise DeprecationWarning()

    def set_node_semantics(self, ns : AcabNodeSemantics):
        raise DeprecationWarning()


#--------------------------------------------------

