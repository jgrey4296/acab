""" The Core Trie Data Structure base """
import logging as root_logger
from .nodes.fact_node import FactNode
from py_rule.abstract.contexts import Contexts
from py_rule.abstract.working_memory import WorkingMemory
from py_rule.abstract.query import Query
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.trie import Trie
from py_rule.error.pyrule_operator_exception import PyRuleOperatorException
from py_rule.error.pyrule_parse_exception import PyRuleParseException
from . import matching
from . import util
from .parsing import FactParser as FP
from .parsing import QueryParser as QP
from .parsing import ActionParser as AP
from .parsing import TransformParser as TP
from .parsing import TotalParser as TotalP
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

    def add(self, s):
        """ Assert multiple facts from a single string """
        if isinstance(s, str):
            assertions = TotalP.parseString(s)
            for x in assertions:
                if util.NEGATION_S in x._data and x._data[util.NEGATION_S]:
                    self._retract_sentence(x)
                else:
                    self._assert_sentence(x)
        elif isinstance(s, Sentence):
            if util.NEGATION_S in s._data and s._data[util.NEGATION_S]:
                self._retract_sentence(s)
            else:
                self._assert_sentence(s)
        else:
            raise PyRuleParseException("Unrecognised addition target: {}".format(type(s)))

    def query(self, s):
        """ Query a string """
        if isinstance(s, Query):
            return self._query_sentence(s)
        elif isinstance(s, str):
            query = QP.parseString(s)
            return self._query_sentence(query)
        elif isinstance(s, Sentence):
            return self._query_sentence(Query([s]))
        else:
            raise PyRuleParseException("Unrecognised query target: {}".format(type(s)))

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
        for newNode in sen:
            self._last_node = self._last_node.insert(newNode)

        self._last_node._set_dirty_chain()

    def _retract_sentence(self, sen):
        """ Retract everything after the end of a sentence """
        assert(isinstance(sen, Sentence))
        # go down to the child, and remove it
        self._clear_last_node()
        factList = sen._words[:]
        lastInList = factList.pop()

        for node in factList:
            self._last_node = self._last_node.get(node)
            if self._last_node is None:
                return

        self._last_node.delete_node(lastInList)

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
                    for bindTarget, val in clause._data[util.FALLBACK_S]:
                        d[bindTarget.value] = val
                updated_contexts._matches += [(x, self._internal_trie._root) for x in failures]

            # If No successful clauses, break
            if bool(updated_contexts) is False:
                logging.debug("A positive clause is false")
                contexts = updated_contexts
                break

            # Update contexts with successes
            contexts = updated_contexts

        for negClause in neg:
            # Return to root unless clause has a head @ binding
            if util.AT_BIND_S in negClause[0]._data:
                reset_start_contexts = contexts.set_all_alts(binding=clause[0]._value)
            else:
                reset_start_contexts = contexts.set_all_alts(target=self._internal_trie._root)

            (result, failures) = self._match_clause(negClause,
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
        currentContexts = contexts
        failures = []
        # Go down from the root by query element:
        # Failure at any point means don't add the updated context

        # For each word of the clause sentence, eg: .a in .a.b.c
        for c in clause:
            logging.info("Testing node: {}".format(repr(c)))
            logging.info("Current Contexts: {}".format(len(currentContexts)))
            if len(currentContexts) == 0:
                break

            alphas, betas, regexs = c.split_tests()
            newContexts = Contexts()

            # test each  active alternative
            for (data, lastNode) in currentContexts._matches:
                tested = False
                newData = None
                newNode = None
                newBindings = []
                if util.AT_BIND_S in c._data:
                    tested = True
                    newNode = lastNode
                    newData = data

                # compare non-bound value, returns (newNode, newData)?
                if not tested:
                    tested, newNode, newData = matching.non_bind_value_match(c, lastNode,
                                                                             betas,
                                                                             regexs, data)

                # compare already bound value, returns (newNode, newData)?
                if not tested:
                    tested, newNode, newData = matching.existing_bind_match(c, lastNode,
                                                                            betas, regexs,
                                                                            data)

                # create new bindings as necessary, returns [(newNode, newData)]
                if not tested:
                    newBindings = matching.create_new_bindings(c, lastNode,
                                                               alphas, betas,
                                                               regexs, data)

                if newData is not None:
                    newContexts.append((newData, newNode))
                elif bool(newBindings):
                    newContexts._matches += [x for x in newBindings if x[0] is not None]
                else:
                    failures.append(data.copy())

                # end of internal loop for an active alternative

            # all alternatives tested for this clause component, update and progress

            currentContexts = newContexts

        # every alternative tested for each clause component,
        # return the final set of contexts
        return (currentContexts, failures)
