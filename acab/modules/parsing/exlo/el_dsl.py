"""
DSL Interface for exclusion logic, to connect it into Acab
"""
from acab.abstract.interfaces.dsl_interface import DSL_Interface
from acab.abstract.parsing import parsers as PU
from acab.error.acab_parse_exception import AcabParseException
from acab.modules.parsing.exlo import util as TPU
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP
from acab.modules.parsing.exlo.parsers import QueryParser as QP
from acab.modules.parsing.exlo.parsers import RuleParser as RP
from acab.modules.parsing.exlo.parsers import TotalParser as TotalP
from acab.modules.parsing.exlo.parsers import TransformParser as TP


class EL_Parser(DSL_Interface):
    """  """

    def assert_parsers(self, bootstrapper):
        """ Provide fragments for other parsers """
        # Core
        # TODO: Make these configurable?
        bootstrapper.add("valbind"             , PU.VALBIND,
                         "sentence.basic"      , FP.BASIC_SEN,
                         "sentence.param"      , FP.PARAM_SEN,
                         "statement.sentence"  , FP.SEN_STATEMENT,
                         "operator.sugar"      , PU.OPERATOR_SUGAR)
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

        return (TotalP.parse_point, QP.parse_point)
