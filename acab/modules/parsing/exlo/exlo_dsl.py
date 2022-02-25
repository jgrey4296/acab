"""
DSL Interface for exclusion logic, to connect it into Acab
"""
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import pyparsing as pp
from acab import GET
from acab.core.parsing import parsers as PU
from acab.core.parsing import pyparse_dsl as ppDSL
from acab.core.parsing.funcs import clear_parser_names, deep_update_names
from acab.error.parse import AcabParseException
from acab.modules.parsing.exlo import util as TPU
from acab.modules.parsing.exlo.parsers import ActionParser as AP
from acab.modules.parsing.exlo.parsers import FactParser as FP
from acab.modules.parsing.exlo.parsers import QueryParser as QP
from acab.modules.parsing.exlo.parsers import RuleParser as RP
from acab.modules.parsing.exlo.parsers import TotalParser as TotalP
from acab.modules.parsing.exlo.parsers import TransformParser as TP

logging      = root_logger.getLogger(__name__)

config = GET()
DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

EXLO_Parser = DSL_Fragment(specs=[DSL_Spec("word.annotation"     , struct=FP.HOTLOAD_ANNOTATIONS,       flags=[DSL_Spec.flag_e.COLLECT]),
                                  # DSL_Spec(TODO FP.HOTLOAD_SEN_HEADS
                                  DSL_Spec("action.statement"    , struct=AP.HOTLOAD_ACTION_STATEMENTS, flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("disallowed.words"    , struct=FP.HOTLOAD_BAD_HEADS,         flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("operators.action"    , struct=AP.HOTLOAD_OPERATORS,         flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("operators.query"     , struct=QP.HOTLOAD_QUERY_OP,          flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("operators.transform" , struct=TP.HOTLOAD_TRANS_OP,          flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("query.statement"     , struct=QP.HOTLOAD_QUERY_SEN,         flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("sentence.ends"       , struct=FP.HOTLOAD_SEN_ENDS,          flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("statement"           , struct=TotalP.HOTLOAD_STATEMENTS,    flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("transform.statement" , struct=TP.HOTLOAD_TRANS_STATEMENTS,  flags=[DSL_Spec.flag_e.COLLECT])],
                           handlers=[
                                     DSL_Handler("sentence"            , FP.SENTENCE),
                                     DSL_Handler("sentence.ends"       , FP.SEN_MACRO),
                                     DSL_Handler("sentence.operator"   , FP.op_sentence),
                                     DSL_Handler("sentence.plural"     , FP.SEN_PLURAL),
                                     DSL_Handler("word.constrained"    , FP.SEN_NO_MODAL),
                                     # Query
                                     DSL_Handler("operators.query"     , PU.OPERATOR_SUGAR),
                                     DSL_Handler("sentence.ends"       , QP.query_sen_end),
                                     DSL_Handler("sentence.ends"       , QP.query_statement),
                                     DSL_Handler("word.annotation"     , QP.word_query_constraint),
                                     # Transform
                                     DSL_Handler("operators.transform" , PU.OPERATOR_SUGAR),
                                     DSL_Handler("sentence.ends"       , TP.transform_statement),
                                     # Action
                                     DSL_Handler("operators.action"    , PU.OPERATOR_SUGAR),
                                     DSL_Handler("sentence.ends"       , AP.action_definition),
                                     # Rule
                                     DSL_Handler("sentence.ends"       , RP.rule),
                                     # Total
                                     DSL_Handler(DEFAULT_HANDLER_SIGNAL, TotalP.parse_point)]
                           )
