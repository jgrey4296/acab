# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
import logging as root_logger

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, TypeVar
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.sentence import Sentence
from acab.abstract.core.value import AcabValue
from acab.abstract.data.contexts import Contexts, CTX_OP
from acab.abstract.data.node import AcabNode
from acab.abstract.data.node_semantics import AcabNodeSemantics
from acab.abstract.data.struct_semantics import AcabStructureSemantics
from acab.abstract.data.structure import DataStructure

import acab.abstract.data.struct_semantics as SSem

from acab.config import AcabConfig


logging = root_logger.getLogger(__name__)
util = AcabConfig.Get()

NEGATION_S = util("Parsing.Structure", "NEGATION_S")
FALLBACK_S = util("Parsing.Structure", "FALLBACK_S")

class BasicNodeSemantics(AcabNodeSemantics):

    def accessible(self, word_data, term):
        potentials = []
        data, node = word_data
        # Expand if variable -> Grab All
        if term.is_var and term.name not in data:
            potentials += node.children
        # Get only matching child if variable is already set
        elif term.is_var:
            assert(term.name in data)
            value = data[term.name]
            if self.contain(node, value):
                potentials.append(node.get_child(value))

        elif self.contain(node, term):
            potentials.append(node.get_child(term))

        return potentials

    def lift(self, word : AcabValue) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        return AcabNode(word)


    def contain(self, node : AcabNode, query_term : AcabValue) -> bool:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))
        in_children = query_term in node
        return in_children

    def get(self, node : AcabNode, query_term : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(query_term, AcabValue))

        if not self.contain(node, query_term):
            return None

        return node.get_child(query_term)

    def add(self, node : AcabNode, to_add : AcabValue) -> AcabNode:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_add, AcabValue))

        if self.contain(node, to_add):
            return self.get(node, to_add)

        new_node = self.lift(to_add)
        node.add_child(new_node)

        return new_node

    def delete(self, node : AcabNode, to_delete : AcabValue) -> Optional[AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(to_delete, AcabValue))

        removed = node.remove_child(to_delete)
        return removed





class BasicTrieSemantics(AcabStructureSemantics):

    def __init__(self, node_semantics, node_type,
                 leaf_type=None, update_data=None, update_func=None):
        super(BasicTrieSemantics, self).__init__(node_semantics, node_type)

        self._leaf_type = leaf_type or self._node_type

        self._update_data = {}
        self._update_func = update_func or (lambda a,b,c: a)

        if update_data is not None:
            self._update_data.update(update_data)

    def add(self, structure : 'Trie', to_add : List[Sentence], leaf_data=None):
        to_add = sorted(to_add,
                        key=lambda x: NEGATION_S in x._data and x._data[NEGATION_S])
        if leaf_data is None:
            leaf_data = {}

        # Get the root
        current = None
        current_path = []
        for sen in to_add:
            current = structure.root
            if NEGATION_S in sen._data and sen._data[NEGATION_S]:
                self.delete(structure, sen)
                continue

            # Add to nodes
            for word in sen:
                current = self._ns.add(current, word)
                self._update_func(current, current_path, self._update_data)
                current_path.append(current)

            current._data.update(leaf_data)
            # TODO Register new nodes with structure weak index

        return current_path[:-1]

    def get(self, structure, sentence):
        assert(isinstance(structure, DataStructure))
        assert(isinstance(sentence, Sentence))

        # Get the root
        current = structure._root
        # Get Nodes
        for word in sentence:
            current = self._ns.get(current, word)
            if current is None:
                return []

        return [current]

    def contain(self, structure, sentence):
        return bool(self.get(structure, sentence))

    def delete(self, structure, sentence):
        retrieved = self.get(structure, sentence[:-1])
        return_list = []
        if bool(retrieved):
            removed = self._ns.delete(retrieved[0], sentence[-1])
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
        except AcabSemanticsException as e:
            logging.warning(str(e))
            initial_context.demote_failures()
            # TODO set context query_history and remainder
        finally:
            return initial_context


    def down(self, data_structure):
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
            contexts.collapse(collapse_on)

    def _negation_semantics(self, contexts, clause):
        if NEGATION_S in clause._data and clause._data[NEGATION_S]:
            contexts.invert()

    def _failure_semantics(self, contexts, clause):
        # add all failures back in, if theres a default value
        if FALLBACK_S in clause._data and bool(clause._data[FALLBACK_S]):
            contexts.promote_failures(clause._data[FALLBACK_S])
        else:
            contexts.demote_failures()


    def _clause_query(self, structure, clause : Sentence, contexts, engine):
        """ Test a single clause,
        annotating contexts upon success and failure """
        logging.debug("Testing Clause: {}".format(repr(clause)))

        clause = self._start_word_semantics(structure, contexts, clause)
        # Go down from the root by query element:
        # For each word of the clause sentence, eg: a. in a.b.word
        collapse_on = set()
        for word in clause:
            logging.info("Testing node: {}".format(repr(word)))
            logging.info("Current Contexts: {}".format(len(contexts)))
            # test each active alternative
            annotations = self._ns._test_word(word, contexts)

            if CTX_OP.collapse in annotations and word.is_var:
                collapse_on.add(word.name)

            # TODO add in context growth restrictions?
            if not bool(contexts):
                break

        self._collapse_semantics(contexts, collapse_on)
        self._negation_semantics(contexts, clause)
        self._failure_semantics(contexts, clause)

        if not bool(contexts):
            raise AcabSemanticException("No successful contexts", clause)

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
