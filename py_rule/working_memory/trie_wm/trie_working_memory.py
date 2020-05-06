""" The Core Trie Data Structure base """
import logging as root_logger

from py_rule.abstract.contexts import Contexts
from py_rule.abstract.working_memory import WorkingMemory
from py_rule.abstract.query import Query
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.trie import Trie
from py_rule.error.pyrule_operator_exception import PyRuleOperatorException
from py_rule.error.pyrule_parse_exception import PyRuleParseException
from py_rule.abstract.printing import util as PrU
from py_rule.abstract.parsing import util as PU

from . import util
from .fact_node import FactNode
from .parsing import ActionParser as AP
from .parsing import FactParser as FP
from .parsing import QueryParser as QP
from .parsing import TotalParser as TotalP
from .parsing import TransformParser as TP
from .parsing import RuleParser as RP

logging = root_logger.getLogger(__name__)


class TrieWM(WorkingMemory):
    """ A Trie based working memory"""

    def __init__(self, init=None):
        """ init is a string of assertions to start the fact base with """
        super().__init__()
        self._internal_trie = Trie(FactNode)
        # TODO: have a parallel listener trie?
        self._last_node = self._internal_trie._root

        if init is not None:
            self.add(init)

    def __str__(self):
        return str(self._internal_trie)

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

    def query(self, query, ctxs=None, engine=None):
        """ Query a string """
        if isinstance(query, str):
            query = QP.parseString(query)
        elif isinstance(query, Sentence):
            query = Query([query])
        if not isinstance(query, Query):
            raise PyRuleParseException("Unrecognised query target: {}".format(type(query)))

        return self._query_sentence(query, ctxs=ctxs)


    def assert_parsers(self, pt):
        # Core
        pt.add("valbind", FP.VALBIND,
               "sentences.basic", FP.BASIC_SEN,
               "sentences.param", FP.PARAM_SEN,
               "statements.sentence", FP.SEN_STATEMENT,
               "operators.sugar", PU.OPERATOR_SUGAR)
        # Query
        pt.add("statements.query", QP.query_statement,
               "query.body", QP.clauses)

        # Transform
        pt.add("transform.body", TP.transforms,
               "statements.transform", TP.transform_statement,
               "transform.rebind", TP.rebind)

        # Action
        pt.add("action.body", AP.actions,
               "statements.action", AP.action_definition)

        # Rule
        pt.add("rule.body", RP.rule_body,
               "statements.rule", RP.rule)

    def query_parsers(self, pt):
        if pt.query("values.*"):
            FP.HOTLOAD_VALUES << pt.query("values.*")
        if pt.query("annotations.*"):
            FP.HOTLOAD_ANNOTATIONS << pt.query("annotations.*")

        QP.HOTLOAD_COMP_OP << pt.query("operators.comparisons.*",
                                       "operators.sugar")

        TP.UNARY_TRANS_OP << pt.query("operators.transform.unary.*",
                                      "operators.sugar")
        TP.BINARY_TRANS_OP << pt.query("operators.transform.binary.*",
                                       "operators.sugar")
        TP.TERNARY_TRANS_OP << pt.query("operators.transform.ternary.*",
                                        "operators.sugar")
        TP.HOTLOAD_TRANS_STATEMENTS << pt.query("operators.transform.statements.*",
                                                "operators.sugar")

        AP.HOTLOAD_OPERATORS << pt.query("operators.action.*")

        TotalP.HOTLOAD_STATEMENTS << pt.query("statements.*")

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
        fact_list = sen.words[:]
        last_in_list = fact_list.pop()

        for node in fact_list:
            self._last_node = self._last_node.get(node)
            if self._last_node is None:
                return

        self._last_node.delete_node(last_in_list)

    def _query_sentence(self, query, ctxs=None):
        """ Query a TrieQuery instance """
        assert(isinstance(query, (Query, Sentence)))
        self._clear_last_node()
        # TODO: handle a passed in context
        initial_context = Contexts(start_node=self._internal_trie._root,
                                   bindings=ctxs)
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
        self._test_clauses(contexts, pos)
        self._test_clauses(contexts, neg, is_negative=True)

        return contexts

    def _test_clauses(self, contexts, clauses, is_negative=False):
        for clause in clauses:
            # Return to root unless clause has a head @ binding
            binding_val = None
            if clause[0]._data[util.BIND_S] == util.AT_BIND_S:
                binding_val = clause[0]._value

            contexts.force_node_position(target=self._internal_trie._root,
                                         binding=binding_val)

            self._match_clause(clause, contexts)

            if is_negative:
                contexts.invert()

            # add all failures back in, if theres a default value
            if util.FALLBACK_S in clause._data and bool(clause._data[util.FALLBACK_S]):
                contexts.promote_failures(clause._data[util.FALLBACK_S])
            else:
                contexts.demote_failures()

            # If No successful clauses, break
            if not bool(contexts):
                clause_type = "positive"
                if is_negative:
                    clause_type = "negative"
                logging.debug("A {} clause has no successful tests".format(clause_type))
                break


    def _match_clause(self, clause, contexts):
        """ Test a single clause, annotating contexts upon success and failure """
        assert(isinstance(clause, Sentence))
        logging.debug("Testing Clause: {}".format(repr(clause)))
        # early exit:
        if not contexts:
            return

        # Go down from the root by query element:
        # For each word of the clause sentence, eg: .a in .a.b.word
        for word in clause:
            logging.info("Testing node: {}".format(repr(word)))
            logging.info("Current Contexts: {}".format(len(contexts)))
            if not bool(contexts):
                break

            if word._data[util.BIND_S] == util.AT_BIND_S:
                continue

            # test each active alternative
            contexts.breadth_apply(word)
