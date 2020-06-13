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

        return self._query_sentence(query, ctxs=ctxs, engine=engine)


    def assert_parsers(self, pt):
        # Core
        pt.add("valbind", FP.VALBIND,
               "sentence.basic", FP.BASIC_SEN,
               "sentence.param", FP.PARAM_SEN,
               "statement.sentence", FP.SEN_STATEMENT,
               "operator.sugar", PU.OPERATOR_SUGAR)
        # Query
        pt.add("statement.query", QP.query_statement,
               "query.body", QP.clauses,
               "query.clause", QP.clause)

        # Transform
        pt.add("transform.body", TP.transforms,
               "statement.transform", TP.transform_statement,
               "transform.rebind", TP.rebind)

        # Action
        pt.add("action.body", AP.actions,
               "statement.action", AP.action_definition)

        # Rule
        pt.add("rule.body", RP.rule_body,
               "statement.rule", RP.rule)

    def query_parsers(self, pt):
        if pt.query("value.*"):
            FP.HOTLOAD_VALUES << pt.query("value.*")
        if pt.query("annotation.*"):
            FP.HOTLOAD_ANNOTATIONS << pt.query("annotation.*")

        if pt.query("query.annotation.*"):
            QP.HOTLOAD_QUERY_ANNOTATIONS << pt.query("query.annotation.*")

        QP.HOTLOAD_QUERY_OP << pt.query("operator.query.*",
                                        "operator.sugar")

        TP.HOTLOAD_TRANS_OP << pt.query("operator.transform.n_ary.*",
                                        "operator.sugar")

        TP.HOTLOAD_TRANS_STATEMENTS << pt.query("operator.transform.statement.*",
                                                "operator.sugar")

        AP.HOTLOAD_OPERATORS << pt.query("operator.action.*",
                                         "operator.sugar")

        TotalP.HOTLOAD_STATEMENTS << pt.query("statement.*")


    def _assert_sentence(self, sen):
        """ Assert a (concrete) sentence of chained facts """
        assert (isinstance(sen, Sentence)), sen
        if self.score_listener(sen.words):
            # TODO: add more listener options
            breakpoint()

        self._clear_last_node()
        for word in sen:
            self._last_node = self._last_node.insert(word)

        self._last_node._set_dirty_chain()

    def _retract_sentence(self, sen):
        """ Retract everything after the end of a (concrete) sentence """
        assert(isinstance(sen, Sentence))
        if self.score_listener(sen.words):
            # TODO: add more listener options
            breakpoint()

        # go down to the child, and remove it
        self._clear_last_node()
        fact_list = sen.words[:]
        last_in_list = fact_list.pop()

        for node in fact_list:
            self._last_node = self._last_node.get(node)
            if self._last_node is None:
                return

        self._last_node.delete_node(last_in_list)

    def _query_sentence(self, query, ctxs=None, engine=None):
        """ Query a TrieQuery instance """
        assert(isinstance(query, (Query, Sentence)))
        should_listen = False
        if isinstance(query, Sentence):
            should_listen = self.score_listener(query.words)
        else:
            should_listen = any([self.score_listener(x.words) for x in query.clauses])

        if should_listen:
            # TODO: add more listener options
            breakpoint()

        self._clear_last_node()
        initial_context = Contexts(start_node=self._internal_trie.root,
                                   bindings=ctxs, engine=engine)
        return self._internal_query(query, initial_context)


    def _clear_last_node(self):
        """ Reset internal memory to point to the root.
        currently only used for retraction
        """
        self._last_node = self._internal_trie._root

    def _internal_query(self, query, ctxs):
        """ Go down the trie, running each defined test,
        annotating bind groups as necessary
        """
        contexts = ctxs
        pos, neg = query.split_clauses()

        logging.debug("Testing clauses: {} {}".format(len(pos), len(neg)))
        self._test_clauses(contexts, pos)
        self._test_clauses(contexts, neg, is_negative=True)

        return contexts

    def _test_clauses(self, contexts, clauses, is_negative=False):
        for clause in clauses:
            self._internal_trie.contextual_query(clause, contexts)

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

        return contexts


    def to_sentences(self):
        return self._internal_trie.to_sentences()
