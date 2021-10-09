"""
DSL Interface for exclusion logic, to connect it into Acab
"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
import pyparsing as pp

from acab.interfaces.dsl import DSL_Fragment_i
from acab.core.parsing import parsers as PU
from acab.error.parse_exception import AcabParseException
from acab.modules.parsing.exlo import util as TPU
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP
from acab.modules.parsing.exlo.parsers import QueryParser as QP
from acab.modules.parsing.exlo.parsers import RuleParser as RP
from acab.modules.parsing.exlo.parsers import TotalParser as TotalP
from acab.modules.parsing.exlo.parsers import TransformParser as TP
from acab.core.parsing.funcs import deep_update_names, clear_parser_names

logging = root_logger.getLogger(__name__)


class EXLO_Parser(DSL_Fragment_i):
    """  """

    def assert_parsers(self, bootstrapper):
        """ Provide fragments for other parsers """
        # Core
        # TODO: make this declarative in EXLO_parser *init*
        bootstrapper.add("word.valbind"                  , PU.VALBIND,
                         "word.constrained"              , FP.SEN_NO_MODAL,
                         "sentence"                      , FP.SENTENCE,
                         "sentence.ends.statement.macro" , FP.SEN_MACRO,
                         "sentence.operator"             , FP.op_sentence,
                         "operator.sugar"                , PU.OPERATOR_SUGAR,
                         "sentence.plural"               , FP.SEN_PLURAL)
        # Query
        bootstrapper.add("sentence.ends.statement.query" , QP.query_statement,
                         "sentence.ends.word.query"      , QP.query_sen_end,
                         "word.annotation.query"         , QP.word_query_constraint)

        # Transform
        bootstrapper.add("sentence.ends.statement.transform" , TP.transform_statement)

        # Action
        bootstrapper.add("sentence.ends.statement.action"    , AP.action_definition)

        # Rule
        bootstrapper.add("sentence.ends.statement.rule"      , RP.rule)


    def query_parsers(self, bootstrapper):
        """ Load in fragments """
        try:
            PU.HOTLOAD_VALUES << bootstrapper.query("word.value.*")
        except Exception:
            logging.debug("No values loaded into DSL")

        try:
            FP.HOTLOAD_ANNOTATIONS << bootstrapper.query("word.annotation.*")
        except Exception:
            logging.debug("No annotations loaded into DSL")

        # TODO FP.HOTLOAD_SEN_HEADS


        FP.HOTLOAD_SEN_ENDS << bootstrapper.query("sentence.ends.statement.*",
                                                  "sentence.ends.word.*")

        QP.HOTLOAD_QUERY_OP << bootstrapper.query("operator.query.*",
                                                  "operator.sugar")

        QP.HOTLOAD_QUERY_SEN << bootstrapper.query("query.statement.*")

        TP.HOTLOAD_TRANS_OP << bootstrapper.query("operator.transform.*",
                                                  "operator.sugar")

        TP.HOTLOAD_TRANS_STATEMENTS << bootstrapper.query("transform.statement.*")

        AP.HOTLOAD_OPERATORS << bootstrapper.query("operator.action.*",
                                                   "operator.sugar")

        AP.HOTLOAD_ACTION_STATEMENTS << bootstrapper.query("action.statement.*")

        TotalP.HOTLOAD_STATEMENTS << bootstrapper.query("statement.*")

        # At this point, parser is constructed, and will not change again
        # however, *can not* deep-copy the parser for multiple versions.
        #
        # Propagate parser names, even through Forwards
        deep_update_names(TotalP.parse_point)
        # Then for next use of parsers to generate their own name again
        clear_parser_names(TotalP.parse_point,
                           TotalP.file_total,
                           TotalP.file_component)

        return TotalP.parse_point


    def set_word_exclusions(self, words:List[str]):
        """
        Utility for setting words that should error if used in particular places.
        ie: repl commands shouldn't try to be used as sentence heads
        """
        if bool(words):
            logging.debug(f"Setting Bad Head Words: {words}")
            FP.HOTLOAD_BAD_HEADS << pp.MatchFirst(words)
