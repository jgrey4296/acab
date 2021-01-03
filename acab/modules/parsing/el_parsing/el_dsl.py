#!/usr/bin/env python3
from acab.abstract.interfaces.dsl_interface import DSL_Interface

from . import ActionParser as AP
from . import FactParser as FP
from . import QueryParser as QP
from . import RuleParser as RP
from . import TotalParser as TotalP
from . import TransformParser as TP
from . import util as TPU


from acab.abstract.parsing import parsers as PU


from acab.error.acab_parse_exception import AcabParseException
class EL_Parser(DSL_Interface):
    """  """

    def assert_parsers(self, pt):
        """ Provide fragments for other parsers """
        # Core
        # TODO: Make these configurable?
        pt.add("valbind"             , PU.VALBIND,
               "sentence.basic"      , FP.BASIC_SEN,
               "sentence.param"      , FP.PARAM_SEN,
               "statement.sentence"  , FP.SEN_STATEMENT,
               "operator.sugar"      , PU.OPERATOR_SUGAR)
        # Query
        pt.add("statement.query"     , QP.query_statement,
               "query.body"          , QP.clauses,
               "query.clause"        , QP.clause)

        # Transform
        pt.add("transform.body"      , TP.transforms,
               "statement.transform" , TP.transform_statement,
               "transform.rebind"    , TP.rebind)

        # Action
        pt.add("action.body"         , AP.actions,
               "statement.action"    , AP.action_definition)

        # Rule
        pt.add("rule.body"           , RP.rule_body,
               "statement.rule"      , RP.rule)


    def query_parsers(self, pt):
        """ Load in fragments """
        try:
            PU.HOTLOAD_VALUES << pt.query("value.*")
        except Exception:
            logging.debug("No values loaded into DSL")

        try:
            FP.HOTLOAD_ANNOTATIONS << pt.query("annotation.*")
        except Exception:
            logging.debug("No annotations loaded into DSL")

        FP.HOTLOAD_QUERY_OP << pt.query("operator.query.*")

        TP.HOTLOAD_TRANS_OP << pt.query("operator.transform.*")

        TP.HOTLOAD_TRANS_STATEMENTS << pt.query("operator.transform.statement.*")

        AP.HOTLOAD_OPERATORS << pt.query("operator.action.*")

        TotalP.HOTLOAD_STATEMENTS << pt.query("statement.*")

        # At this point, parser is constructed, and will not change again
        # however, can't deep-copy the parser for multiple versions

        return (TotalP.parse_point, QP.parse_point)
