# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from copy import deepcopy
from dataclasses import dataclass, field, InitVar, replace
from fractions import Fraction
from re import Pattern
from uuid import uuid1, UUID
from weakref import ref
import logging as root_logger


from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.core.contexts import Contexts
from acab.abstract.rule.structure import DataStructure
from acab.abstract.core.node_semantics import AcabNodeSemantics

from acab.abstract.interfaces import semantics_interface as SI

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.abstract.config.config import AcabConfig

util = AcabConfig.Get()

NEGATION_S   = util.value("Value.Structure", "NEGATION")
CONSTRAINT_S = util.value("Value.Structure", "CONSTRAINT")
AT_BIND_S    = util.value("Value.Structure", "AT_BIND")
ROOT_S       = util.value("Data", "ROOT")

@dataclass
class AcabStructureSemantics(AcabValue, SI.SemanticInterface, SI.StructureSemantics):
    # TODO Locate listeners in semantics not WM

    node_semantics: Dict[AcabNode, AcabNodeSemantics] = field(default_factory=dict)
    value_pairings: Dict[AcabValue, Tuple[AcabNode, Dict[Any, Any]]] = field(default_factory=dict)

    def __post_init__(self):
        """
        Structure Semantics define the behaviour of a *collection* of nodes,
        and uses two mappings:
        1) Node -> Node Semantics
        2) Value -> Node

        """
        super(AcabStructureSemantics, self).__post_init__()

        # TODO: verify value -> node_c -> semantic chains

    def init_struct(self, struct):
        """
        Create a basic root node / entry point for a data structure
        """
        constructor, u_data = self.value_pairings[AcabValue]
        node_semantics = self.retrieve_semantics(constructor)
        return node_semantics.up(AcabValue(ROOT_S), constructor)

    def retrieve_semantics(self, node):
        """
        Map node -> its semantics
        """
        assert(isinstance(node, type))
        # TODO should I be using my type instances for semantics?
        curr = node
        retrieved = None
        descendents_to_update = []
        while retrieved is None and curr not in (object, None):
            if curr in self.node_semantics:
                retrieved = self.node_semantics[curr]
            else:
                curr = curr.__base__
                descendents_to_update.append(curr)

        if retrieved is None:
            raise AcabSemanticException("Missing Node Semantic Binding for: {}".format(node), None)

        if len(descendents_to_update) > 1:
            self.node_semantics.update({x : retrieved for x in descendents_to_update})

        return retrieved

    def value_constructor(self, value):
        """
        Get the most applicable lifting from value -> node
        """
        assert(isinstance(value, type))
        # TODO should I be using my type instances for semantics?
        curr = value
        retrieved = None
        descendents_to_update = []
        while retrieved is None and curr not in (object, None):
            if curr in self.value_pairings:
                retrieved = self.value_pairings[curr]
            else:
                curr = curr.__base__
                descendents_to_update.append(curr)


        if retrieved is None:
            raise AcabSemanticException("Missing Construction data for: {}".format(value),
                                        None)

        if len(descendents_to_update) > 1:
            self.value_pairings.update({x : retrieved for x in descendents_to_update})

        return retrieved



    def delete(self, structure, sentence) -> List[AcabNode]:
        """ Remove a sentence from the structure """
        raise NotImplementedError()



    def filter_candidates(self, structure, candidates, match_func):
        raise NotImplementedError()


#--------------------------------------------------

