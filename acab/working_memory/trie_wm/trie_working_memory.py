""" The Core Trie Data Structure base """
import logging as root_logger
from copy import deepcopy

from acab.config import AcabConfig

from acab.abstract.printing import util as PrU
from acab.abstract.parsing import util as PU
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.contexts import Contexts
from acab.abstract.data.trie import Trie
from acab.abstract.rule.query import Query

from acab.abstract.engine.working_memory import WorkingMemory

from acab.error.acab_operator_exception import AcabOperatorException
from acab.error.acab_parse_exception import AcabParseException

from .fact_node import FactNode
from .parsing import ActionParser as AP
from .parsing import FactParser as FP
from .parsing import QueryParser as QP
from .parsing import TotalParser as TotalP
from .parsing import TransformParser as TP
from .parsing import RuleParser as RP

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

NEGATION_S = util("Parsing.Structure", "NEGATION_S")
FALLBACK_S = util("Parsing.Structure", "FALLBACK_S")

class TrieWM(WorkingMemory):
    """ A Trie based working memory"""
    # TODO set default semantics class

    def __init__(self, init=None):
        """ init is a string of assertions to start the fact base with """
        # TODO enable passing of starting node semantics
        super().__init__(init)
        self._internal_trie = Trie(FactNode)
        # parser : PyParsing.ParserElement
        self._main_parser = None
        self._query_parser = None

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
            raise AcabOperatorException("Incorrect Eq arg: {}".format(type(other)))


    def add(self, data, leaf=None, semantics=None):
        """ Assert multiple facts from a single string """
        assertions = None
        if isinstance(data, str):
            assertions = TotalP.parseString(data, self._main_parser)
        elif isinstance(data, Sentence):
            assertions = [data]
        else:
            raise AcabParseException("Unrecognised addition target: {}".format(type(data)))

        if len(assertions) == 1 and leaf:
            assertions = [assertions[0].attach_statement(leaf)]

        return semantics.add(self._internal_trie, assertions)


    def query(self, query, ctxs=None, engine=None):
        """ Query a string, return a Contexts """
        if isinstance(query, str):
            query = QP.parseString(query, self._query_parser)
        elif isinstance(query, Sentence):
            query = Query([query])
        if not isinstance(query, Query):
            raise AcabParseException("Unrecognised query target: {}".format(type(query)))

        return self._query_sentence(query, ctxs=ctxs, engine=engine)


    def assert_parsers(self, pt):
        # Core
        # TODO: Make these configurable?
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
        try:
            FP.HOTLOAD_VALUES << pt.query("value.*")
        except Exception:
            logging.debug("No values loaded into DSL")

        try:
            FP.HOTLOAD_ANNOTATIONS << pt.query("annotation.*")
        except Exception:
            logging.debug("No annotations loaded into DSL")

        try:
            QP.HOTLOAD_QUERY_ANNOTATIONS << pt.query("query.annotation.*")
        except Exception:
            logging.debug("No query annotations loaded into DSL")

        QP.HOTLOAD_QUERY_OP << pt.query("operator.query.*",
                                        "operator.sugar")

        TP.HOTLOAD_TRANS_OP << pt.query("operator.transform.n_ary.*",
                                        "operator.sugar")

        TP.HOTLOAD_TRANS_STATEMENTS << pt.query("operator.transform.statement.*",
                                                "operator.sugar")

        AP.HOTLOAD_OPERATORS << pt.query("operator.action.*",
                                         "operator.sugar")

        TotalP.HOTLOAD_STATEMENTS << pt.query("statement.*")

        # At this point, parser is constructed, and will not change again
        # freeze the parser with Deep Copy
        # This enables having multiple working memories with non-interacting parsers
        self._main_parser = deepcopy(TotalP.parse_point)
        self._query_parser = deepcopy(QP.parse_point)


    def to_sentences(self, semantics):
        return semantics.down(self._internal_trie)
