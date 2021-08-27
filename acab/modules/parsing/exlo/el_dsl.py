"""
DSL Interface for exclusion logic, to connect it into Acab
"""
import logging as root_logger

from acab.abstract.interfaces.dsl import DSL_Fragment_i
from acab.abstract.parsing import parsers as PU
from acab.error.acab_parse_exception import AcabParseException
from acab.modules.parsing.exlo import util as TPU
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP
from acab.modules.parsing.exlo.parsers import QueryParser as QP
from acab.modules.parsing.exlo.parsers import RuleParser as RP
from acab.modules.parsing.exlo.parsers import TotalParser as TotalP
from acab.modules.parsing.exlo.parsers import TransformParser as TP
from acab.abstract.parsing.funcs import deep_update_names

logging = root_logger.getLogger(__name__)


class EL_Parser(DSL_Fragment_i):
    """  """

    def assert_parsers(self, bootstrapper):
        """ Provide fragments for other parsers """
        # Core
        # TODO: Make these configurable?
        bootstrapper.add("valbind"             , PU.VALBIND,
                         "sentence.basic"      , FP.BASIC_SEN,
                         "sentence.param"      , FP.PARAM_SEN,
                         "statement.sentence"  , FP.SEN_STATEMENT,
                         "operator.sugar"      , PU.OPERATOR_SUGAR,
                         "sentence.plural"     , FP.PARAM_SEN_PLURAL)
        # Query
        bootstrapper.add("statement.query"     , QP.query_statement,
                         "query.body"          , QP.clauses,
                         "query.clause"        , QP.clause)

        # Transform
        bootstrapper.add("transform.body"      , TP.transforms,
                         "statement.transform" , TP.transform_statement,
                         "transform.rebind"    , TP.rebind)

        # Action
        bootstrapper.add("action.body"         , AP.actions,
                         "statement.action"    , AP.action_definition)

        # Rule
        bootstrapper.add("rule.body"           , RP.rule_body,
                         "statement.rule"      , RP.rule)


    def query_parsers(self, bootstrapper):
        """ Load in fragments """
        try:
            PU.HOTLOAD_VALUES << bootstrapper.query("value.*")
        except Exception:
            logging.debug("No values loaded into DSL")

        try:
            FP.HOTLOAD_ANNOTATIONS << bootstrapper.query("annotation.*")
        except Exception:
            logging.debug("No annotations loaded into DSL")

        FP.HOTLOAD_QUERY_OP << bootstrapper.query("operator.query.*")

        TP.HOTLOAD_TRANS_OP << bootstrapper.query("operator.transform.*")

        TP.HOTLOAD_TRANS_STATEMENTS << bootstrapper.query("operator.transform.statement.*")

        AP.HOTLOAD_OPERATORS << bootstrapper.query("operator.action.*")

        TotalP.HOTLOAD_STATEMENTS << bootstrapper.query("statement.*")
        # At this point, parser is constructed, and will not change again
        # however, *can not* deep-copy the parser for multiple versions
        deep_update_names(TotalP.parse_point)
        if hasattr(TotalP.parse_point.exprs[0].exprs[1], "name"):
            delattr(TotalP.parse_point.exprs[0].exprs[1], "name")
        TotalP.parse_point.strRepr = None

        return (TotalP.parse_point, QP.parse_point)
