#!/usr/bin/env python3
import logging as root_logger

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue, AcabStatement
from acab.abstract.data.contexts import Contexts, CTX_OP
from acab.abstract.data.node import AcabNode
from acab.abstract.data.node_semantics import AcabNodeSemantics
from acab.abstract.data.struct_semantics import AcabStructureSemantics
from acab.abstract.data.structure import DataStructure
from acab.abstract.rule.query import QueryComponent

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.error.acab_base_exception import AcabBaseException

from acab.config import AcabConfig

logging = root_logger.getLogger(__name__)
util = AcabConfig.Get()

CONSTRAINT_S = util("Parsing.Structure", "CONSTRAINT_S")
NEGATION_S = util("Parsing.Structure", "NEGATION_S")
QUERY_FALLBACK_S = util("Parsing.Structure", "QUERY_FALLBACK_S")

class BasicTrieSemantics(AcabStructureSemantics):
    """
    """

    def __init__(self,
                 node_semantics : Dict[AcabNode, AcabNodeSemantics],
                 value_pairings: Dict[AcabValue, Tuple[AcabNode, Dict[Any, Any], Callable]],
                 sentence_sort=None):
        """
        Update function format: λ current_node
        """
        assert(isinstance(node_semantics, dict))
        super(BasicTrieSemantics, self).__init__(node_semantics,value_pairings)

        # Used to update every node
        self._sentence_sort = lambda x: NEGATION_S in x._data and x._data[NEGATION_S]
        if sentence_sort is not None:
            self._sentence_sort = sentence_sort

    def add(self, structure : 'Trie',
            to_add : List[Sentence],
            leaf_data=None,
            leaf_func=None,
            context_data=None):
        to_add = sorted(to_add, key=self._sentence_sort)

        # Get the root
        all_paths = []
        for sen in to_add:
            current_path = []
            current = structure.root
            if NEGATION_S in sen._data and sen._data[NEGATION_S]:
                self.delete(structure, sen)
                continue

            # Add to nodes
            for word in sen:
                """
                When going down the trie, the current node determines the
                semantics for progressing
                """
                node_c, u_data, u_func = self.value_constructor(type(word))
                node_semantics = self.retrieve_semantics(type(current))
                is_new_node, current = node_semantics.add(current, word, node_c)
                if u_func is None:
                    u_func = node_c._default_setup
                if is_new_node:
                    u_func(current, current_path, u_data, context_data)
                current_path.append(current)


            # Run the leaf update function if there is one:
            if leaf_func is not None:
                leaf_func(current, leaf_data, context_data)
            elif leaf_data is not None:
                current._data.update(leaf_data)

            all_paths.append(current_path)
            # TODO Register new nodes with structure weak index

        return all_paths

    def get(self, structure, sentence):
        assert(isinstance(structure, DataStructure))
        assert(isinstance(sentence, Sentence))

        # Get the root
        current = structure._root
        # Get Nodes
        for word in sentence:
            node_semantics = self.retrieve_semantics(type(current))
            current = node_semantics.get(current, word)
            if current is None:
                return []

        return [current]

    def contain(self, structure, sentence):
        return bool(self.get(structure, sentence))

    def delete(self, structure, sentence):
        retrieved = self.get(structure, sentence[:-1])
        return_list = []
        if bool(retrieved):
            # Use the semantics of the penultimate node
            # to delete the target node
            node_semantics = self.retrieve_semantics(type(retrieved[0]))
            removed = node_semantics.delete(retrieved[0], sentence[-1])
            if removed is not None:
                return_list.append(removed)

        return return_list

    def query(self, structure, query, ctxs, engine):
        initial_context = Contexts(start_node=structure.root,
                                   bindings=ctxs, engine=engine)

        pos, neg = query.split_clauses()
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


    def down(self, data_structure, leaf_predicate=None):
        # TODO leaf_predicate
        output = []
        queue = [([], x) for x in data_structure._root]

        while bool(queue):
            curr_path, current_node = queue.pop(0)
            total_path = curr_path + [current_node.value]
            if not bool(current_node) or isinstance(current_node.value, AcabStatement):
                if leaf_predicate is None or leaf_predicate(current_node):
                    as_sentence = Sentence(total_path)
                    output.append(as_sentence)

            if bool(current_node):
                queue += [(total_path, x) for x in current_node]

        return output


    def _start_word_semantics(self, structure, contexts, clause):
        binding = None
        if clause[0].is_at_var:
            binding = clause[0].value

        contexts.force_node_position(target=structure.root,
                                     binding=binding)

        if binding is not None:
            return clause[1:]

        return clause


    def _collapse_semantics(self, ctxs, collapse_set):
         if bool(collapse_set):
            ctxs.collapse(collapse_set)

    def _negation_semantics(self, contexts, clause):
        if NEGATION_S in clause._data and clause._data[NEGATION_S]:
            contexts.invert()

    def _failure_semantics(self, contexts, clause):
        # add all failures back in, if theres a default value
        if QUERY_FALLBACK_S in clause._data and bool(clause._data[QUERY_FALLBACK_S]):
            contexts.promote_failures(clause._data[QUERY_FALLBACK_S])
        else:
            contexts.demote_failures()


    def _clause_query(self, structure, clause : Sentence, contexts, engine):
        """ Test a single clause,
        annotating contexts upon success and failure """
        logging.debug("Testing Clause: {}".format(repr(clause)))

        clause = self._start_word_semantics(structure, contexts, clause)
        #  o down from the root by query element:
        # For each word of the clause sentence, eg: a. in a.b.word
        collapse_on = set()
        for word in clause:
            tests, annotations = self._validate_and_split_constraints(word, ctx=contexts, engine=engine)
            if CTX_OP.collapse in annotations and word.is_var:
                collapse_on.add(word.name)

            logging.info("Testing node: {}".format(repr(word)))
            logging.info("Current Contexts: {}".format(len(contexts)))
            node_groups, ancestor_tracker = contexts.group_by_type()
            group_semantics = {self.retrieve_semantics(x) : y for x,y in node_groups.items()}

            # test each active alternative
            passing_candidates = [r for x,y in group_semantics.items() for r in x.test_candidates(word, y, tests, engine)]

            # Merge then add
            contexts.clear()
            contexts.append(passing_candidates, ancestor_tracker)

            # TODO add in context growth restrictions?
            if not bool(contexts):
                break

        self._collapse_semantics(contexts, collapse_on)
        self._negation_semantics(contexts, clause)
        self._failure_semantics(contexts, clause)

        if not bool(contexts):
            raise AcabSemanticException("No successful contexts", clause.pprint())

        return contexts


    def filter_candidates(self, target_pattern, candidates, match_func):
        """ Filter candidates using match_func to compare
        against this data_structure

        Where a Match = [(PatternNode, MatchNode)]
        Return [Match]

        match_func : Node -> [Node] -> [Node]
        """
        assert(isinstance(target_pattern, DataStructure))

        if isinstance(candidates, DataStructure):
            candidates = candidates.root

        if not isinstance(candidates, AcabNode):
            raise AcabBaseException()

        final_matches = []
        pattern_nodes = list(candidates.children)
        # (current pattern position, available choices, match state)
        queue = [(word, pattern_nodes, []) for word in target_pattern.root.children]

        while bool(queue):
            current, available_nodes, match_state = queue.pop(0)

            matching_nodes = match_func(current, available_nodes)
            for node in matching_nodes:
                next_match_state = match_state + [(current, node)]

                if bool(current):
                    next_available = list(node.children)
                    next_patterns = list(current.children)
                    queue += [(word, next_available, next_match_state) for word in next_patterns]
                else:
                    final_matches.append(next_match_state)

        return final_matches


    def _validate_and_split_constraints(self, word, ctx=None, engine=None):
        """ Split tests into (alphas, betas, sub_binds),
        Also connect Components to actual operators
        """
        if CONSTRAINT_S not in word._data:
            return (None, set())

        constraints = word._data[CONSTRAINT_S]
        annotations = set()
        callable_annotations = []
        alphas = []
        betas = []
        sub_binds = []
        variable_ops = []
        for c in constraints:
            if not isinstance(c, QueryComponent) and hasattr(c, "__call__"):
                callable_annotations.append(c)
            # intentionally not elif:
            if not isinstance(c, QueryComponent):
                annotations.add(c)
            # intentionally elifs:
            elif c.is_var:
                variable_ops.append(c)
            elif c.is_sub_bind_test:
                sub_binds.append(c)
            elif c.is_alpha_test:
                alphas.append(c)
            else:
                betas.append(c)

        return ((alphas, betas, sub_binds, callable_annotations, variable_ops), annotations)
