#!/usr/bin/env python3
import logging as root_logger

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.values import Sentence
from acab.abstract.core.values import AcabValue, AcabStatement
from acab.abstract.core.node import AcabNode
from acab.modules.semantics.node_testing import AcabNodeTestSemantics
from acab.abstract.core.acab_struct import AcabStruct
from acab.abstract.core.production_abstractions import ProductionContainer

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.error.acab_base_exception import AcabBaseException

from acab.abstract.config.config import AcabConfig
import acab.abstract.interfaces.semantic_interfaces as SI
from acab.abstract.interfaces.data_interfaces import StructureInterface

from .util import split_clauses

logging = root_logger.getLogger(__name__)
config = AcabConfig.Get()

CONSTRAINT_S = config.value("Value.Structure", "CONSTRAINT")
NEGATION_S = config.value("Value.Structure", "NEGATION")
QUERY_FALLBACK_S = config.value("Value.Structure", "QUERY_FALLBACK")
DEFAULT_SETUP_S = config.value("Data", "DEFAULT_SETUP_METHOD")
DEFAULT_UPDATE_S = config.value("Data", "DEFAULT_UPDATE_METHOD")

T = TypeVar('T')
T2 = TypeVar('T2')

Node          = 'AcabNode'
Sentence      = 'Sentence'
Printable     = 'Printable'
Value         = 'AcabValue'
Structure     = 'AcabStruct'
Engine        = 'Engine'
Contexts      = 'Contexts'
SemanticUnion = Union['IndependentSemantics', 'DependentSemantics']


class BasicTrieSemantics(SI.SemanticsMap,
                         SI.DependentSemantics):
    """
    """

    def __init__(self,
                 mapping: Dict[T, SemanticUnion],
                 lifting: Dict[Value, T],
                 sentence_sort=None,
                 ):
        """
        Update function format: λ current_node
        """
        assert isinstance(mapping, dict)
        super(BasicTrieSemantics, self).__init__(mapping=mapping, lifting=lifting)

        # Used to update every node
        self._sentence_sort = lambda x: NEGATION_S in x.data and x.data[NEGATION_S]
        if sentence_sort is not None:
            self._sentence_sort = sentence_sort


    def add(self,
            structure: "Trie",
            to_add: List[Sentence],
            leaf_data=None,
            leaf_func=None,
            context_data=None,
            ):

        to_add = sorted(to_add, key=self._sentence_sort)
        if context_data is None:
            context_data = {}

        # Get the root
        all_paths = []
        for sen in to_add:
            current_path = []
            current = structure.root
            if NEGATION_S in sen.data and sen.data[NEGATION_S]:
                self.delete(structure, sen)
                continue

            # Add to nodes
            for word in sen:
                """
                When going down the trie, the current node determines the
                semantics for progressing
                """
                node_c, u_data = self.value_constructor(type(word))
                mapping = self.retrieve_semantics(type(current))
                if isinstance(mapping, tuple):
                    # TODO
                    # Gotten something bad
                    breakpoint()
                    logging.warning("Retrieve semantics has issues")

                is_new_node, current = mapping.add(current, word, node_c)
                # run a setup on the node if necessary
                # TODO shift this into node semantics
                if is_new_node:
                    getattr(node_c, DEFAULT_SETUP_S)(
                        current, current_path, u_data, context_data
                    )
                else:
                    getattr(node_c, DEFAULT_UPDATE_S)(
                        current, current_path, u_data, context_data
                    )
                current_path.append(current)

            # Run the leaf update function if there is one:
            if leaf_func is not None:
                leaf_func(current, leaf_data, context_data)
            elif leaf_data is not None:
                current.data.update(leaf_data)

            all_paths.append(current_path)
            # TODO Register new nodes with structure weak index

        return all_paths

    def contain(self, structure, sentence):
        return bool(self._get(structure, sentence))

    def delete(self, structure, sentence):
        retrieved = self._get(structure, sentence[:-1])
        return_list = []
        if bool(retrieved):
            # Use the semantics of the penultimate node
            # to delete the target node
            mapping = self.retrieve_semantics(type(retrieved[0]))
            removed = mapping.delete(retrieved[0], sentence[-1])
            if removed is not None:
                return_list.append(removed)

        return return_list

    def query(self, structure, query, ctxs, engine):
        initial_context = Contexts(
            start_node=structure.root, bindings=ctxs, engine=engine
        )

        clauses = []
        if isinstance(query, Sentence):
            clauses.append(query)
        else:
            assert isinstance(query, ProductionContainer)
            clauses += query.clauses

        # TODO refactor this:
        pos, neg = split_clauses(clauses)
        try:
            for clause in pos:
                self._clause_query(structure, clause, initial_context, engine)
            for clause in neg:
                self._clause_query(structure, clause, initial_context, engine)
        except AcabSemanticException as e:
            logging.debug(str(e))
            initial_context.demote_failures()
            # TODO set context query_history and remainder
        finally:
            return initial_context


    def filter_candidates(self, target_pattern, candidates, match_func):
        """ Filter candidates using match_func to compare
        against this data_structure

        Where a Match = [(PatternNode, MatchNode)]
        Return [Match]

        match_func : Node -> [Node] -> [Node]
        """
        # TODO check this
        assert(isinstance(target_pattern, AcabStruct))

        if isinstance(candidates, AcabStruct):
            candidates = candidates.root

        if not isinstance(candidates, AcabNode):
            raise AcabBaseException()

        final_matches = []
        pattern_nodes = list(candidates.children.values())
        # (current pattern position, available choices, match state)
        queue = [(word, pattern_nodes, []) for word in target_pattern.root.children.values()]

        while bool(queue):
            current, available_nodes, match_state = queue.pop(0)

            matching_nodes = match_func(current, available_nodes)
            for node in matching_nodes:
                next_match_state = match_state + [(current, node)]

                if bool(current):
                    next_available = list(node.children.values())
                    next_patterns = list(current.children.values())
                    queue += [
                        (word, next_available, next_match_state)
                        for word in next_patterns
                    ]
                else:
                    final_matches.append(next_match_state)

        return final_matches

    def accessible(self, node: T,
                   data: Dict[Any, Any],
                   term: Value) -> List[T]:
        """
        Retrieve a list of all nodes accessible from this node,
        according to a constraint term
        """
        pass


    def make(self, val: T):
        pass



    def test_candidates(self, term, candidate_triple, tests, engine: T2) -> List[Any]:
        pass


    def equal(self, other):
        pass


    def value_constructor(self, value):
        pass
