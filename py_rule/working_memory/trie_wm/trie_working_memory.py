""" The Core Trie Data Structure base """
import logging as root_logger

from py_rule.abstract.contexts import Contexts
from py_rule.abstract.working_memory import WorkingMemory
from py_rule.abstract.query import Query
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.trie import Trie
from py_rule.error.pyrule_operator_exception import PyRuleOperatorException
from py_rule.error.pyrule_parse_exception import PyRuleParseException

from . import matching
from . import util
from .nodes.fact_node import FactNode
from .parsing import ActionParser as AP
from .parsing import FactParser as FP
from .parsing import QueryParser as QP
from .parsing import TotalParser as TotalP
from .parsing import TransformParser as TP

logging = root_logger.getLogger(__name__)


class TrieWM(WorkingMemory):
    """ A Trie based working memory"""

    def __init__(self, init=None):
        """ init is a string of assertions to start the fact base with """
        super().__init__()
        self._internal_trie = Trie(FactNode)
        self._last_node = self._internal_trie._root
        if init is not None:
            self.add(init)

    def __eq__(self, other):
        if isinstance(other, TrieWM):
            return self._internal_trie._root == other._internal_trie._root
        elif isinstance(other, Trie):
            return self._internal_trie._root == other._root
        else:
            raise PyRuleOperatorException("Incorrect Eq arg: {}".format(type(other)))

    def add(self, data):
        """ Assert multiple facts from a single string """
        if isinstance(data, str):
            assertions = TotalP.parseString(data)
            for x in assertions:
                if util.NEGATION_S in x._data and x._data[util.NEGATION_S]:
                    self._retract_sentence(x)
                else:
                    self._assert_sentence(x)
        elif isinstance(data, Sentence):
            if util.NEGATION_S in data._data and data._data[util.NEGATION_S]:
                self._retract_sentence(data)
            else:
                self._assert_sentence(data)
        else:
            raise PyRuleParseException("Unrecognised addition target: {}".format(type(data)))

    def query(self, data):
        """ Query a string """
        if isinstance(data, Query):
            return self._query_sentence(data)
        elif isinstance(data, str):
            query = QP.parseString(data)
            return self._query_sentence(query)
        elif isinstance(data, Sentence):
            return self._query_sentence(Query([data]))
        else:
            raise PyRuleParseException("Unrecognised query target: {}".format(type(data)))

    def __str__(self):
        return str(self._internal_trie)


    # Internal Methods:
    def _insert_into_values_parser(self, parser):
        FP.HOTLOAD_VALUES << parser

    def _insert_into_statement_parser(self, parser):
        TotalP.HOTLOAD_STATEMENTS << parser

    def _insert_into_annotations_parser(self, parser):
        FP.HOTLOAD_ANNOTATIONS << parser

    def _build_operator_parser(self):
        """ Trigger the building of operators,
        *after* modules have been loaded
        """
        AP.build_operators()
        QP.build_operators()
        TP.build_operators()

    def _assert_sentence(self, sen):
        """ Assert a sentence of chained facts """
        assert (isinstance(sen, Sentence)), sen
        self._clear_last_node()
        for new_node in sen:
            self._last_node = self._last_node.insert(new_node)

        self._last_node._set_dirty_chain()

    def _retract_sentence(self, sen):
        """ Retract everything after the end of a sentence """
        assert(isinstance(sen, Sentence))
        # go down to the child, and remove it
        self._clear_last_node()
        fact_list = sen._words[:]
        last_in_list = fact_list.pop()

        for node in fact_list:
            self._last_node = self._last_node.get(node)
            if self._last_node is None:
                return

        self._last_node.delete_node(last_in_list)

    def _query_sentence(self, query):
        """ Query a TrieQuery instance """
        assert(isinstance(query, (Query, Sentence)))
        self._clear_last_node()
        initial_context = Contexts.initial(self._internal_trie._root)
        return self._internal_query(query, initial_context)

    def _clear_last_node(self):
        """ Reset internal memory to point to the root.
        currently only used for retraction
        """
        self._last_node = self._internal_trie._root

    def _internal_query(self, query, ctxs):
        """ Go down the trie, running each test as necessary
        annotating contexts as necessary
        """
        contexts = ctxs
        pos, neg = query.split_clauses()

        logging.debug("Testing clauses: {} {}".format(len(pos), len(neg)))
        for clause in pos:
            # Return to root unless clause has a head @ binding
            if util.AT_BIND_S in clause[0]._data:
                reset_start_contexts = contexts.set_all_alts(binding=clause[0]._value)
            else:
                reset_start_contexts = contexts.set_all_alts(target=self._internal_trie._root)

            (updated_contexts, failures) = self._match_clause(clause,
                                                              reset_start_contexts)
            # add all failures back in, if theres a default value
            if util.FALLBACK_S in clause._data and bool(clause._data[util.FALLBACK_S]):
                for d in failures:
                    for bind_target, val in clause._data[util.FALLBACK_S]:
                        d[bind_target.value] = val
                        updated_contexts._matches += [(x, self._internal_trie._root) for x in failures]

            # If No successful clauses, break
            if bool(updated_contexts) is False:
                logging.debug("A positive clause is false")
                contexts = updated_contexts
                break

            # Update contexts with successes
            contexts = updated_contexts

        for neg_clause in neg:
            # Return to root unless clause has a head @ binding
            if util.AT_BIND_S in neg_clause[0]._data:
                reset_start_contexts = contexts.set_all_alts(binding=neg_clause[0]._value)
            else:
                reset_start_contexts = contexts.set_all_alts(target=self._internal_trie._root)

            (result, failures) = self._match_clause(neg_clause,
                                                    reset_start_contexts)
            logging.debug("neg result: {}".format(str(result)))
            # If negative clauses are true, break

            if bool(result) is True:
                logging.debug("A Negative clause is true")
                contexts.fail()
                break

        return contexts

    def _match_clause(self, clause, contexts):
        """ Test a single clause, annotating contexts upon success and failure """
        assert(isinstance(clause, Sentence))
        logging.debug("Testing Clause: {}".format(repr(clause)))
        # early exit:
        if not contexts:
            return (contexts, [])
        current_contexts = contexts
        failures = []
        # Go down from the root by query element:
        # Failure at any point means don't add the updated context

        # For each word of the clause sentence, eg: .a in .a.b.c
        for c in clause:
            logging.info("Testing node: {}".format(repr(c)))
            logging.info("Current Contexts: {}".format(len(current_contexts)))
            if len(current_contexts) == 0:
                break

            alphas, betas, regexs = c.split_tests()
            new_contexts = Contexts()

            # test each  active alternative
            for (data, last_node) in current_contexts._matches:
                tested = False
                new_data = None
                new_node = None
                new_bindings = []
                if util.AT_BIND_S in c._data:
                    tested = True
                    new_node = last_node
                    new_data = data

                # compare non-bound value, returns (new_node, new_data)?
                if not tested:
                    tested, new_node, new_data = matching.non_bind_value_match(c, last_node,
                                                                               betas,
                                                                               regexs, data)

                # compare already bound value, returns (new_node, new_data)?
                if not tested:
                    tested, new_node, new_data = matching.existing_bind_match(c, last_node,
                                                                              betas, regexs,
                                                                              data)

                # create new bindings as necessary, returns [(new_node, new_data)]
                if not tested:
                    new_bindings = matching.create_new_bindings(c, last_node,
                                                                alphas, betas,
                                                                regexs, data)

                if new_data is not None:
                    new_contexts.append((new_data, new_node))
                elif bool(new_bindings):
                    new_contexts._matches += [x for x in new_bindings if x[0] is not None]
                else:
                    failures.append(data.copy())

                # end of internal loop for an active alternative

            # all alternatives tested for this clause component, update and progress

            current_contexts = new_contexts

        # every alternative tested for each clause component,
        # return the final set of contexts
        return (current_contexts, failures)
