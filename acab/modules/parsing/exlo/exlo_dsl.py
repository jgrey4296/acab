"""
DSL Interface for exclusion logic, to connect it into Acab
"""
import logging as logmod
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

logging      = logmod.getLogger(__name__)

config = GET()
DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

DSL_Fragment = ppDSL.DSL_Fragment
DSL_Spec     = ppDSL.PyParse_Spec
DSL_Handler  = ppDSL.PyParse_Handler

Link_Signal_To = lambda x, y: DSL_Spec(x, struct=y, flags=[DSL_Spec.flag_e.COLLECT])

EXLO_Parser = DSL_Fragment(specs=[Link_Signal_To("action.statement"         , AP.HOTLOAD_ACTION_STATEMENTS),
                                  Link_Signal_To("disallowed.words"         , FP.HOTLOAD_BAD_HEADS),
                                  Link_Signal_To("operators.action"         , AP.HOTLOAD_OPERATORS),
                                  Link_Signal_To("operators.query"          , QP.HOTLOAD_QUERY_OP),
                                  Link_Signal_To("operators.transform"      , TP.HOTLOAD_TRANS_OP),
                                  Link_Signal_To("query.statement"          , QP.HOTLOAD_QUERY_SEN),
                                  Link_Signal_To("sentence.annotation.end"  , FP.HOTLOAD_SEN_ENDS),
                                  Link_Signal_To("sentence.annotation.head" , FP.HOTLOAD_SEN_HEADS),
                                  Link_Signal_To("sentence.annotation.post" , FP.HOTLOAD_SEN_POSTS),
                                  Link_Signal_To("sentence.ends"            , FP.HOTLOAD_SEN_ENDS),
                                  Link_Signal_To("statement"                , TotalP.HOTLOAD_STATEMENTS),
                                  Link_Signal_To("transform.statement"      , TP.HOTLOAD_TRANS_STATEMENTS),
                                  Link_Signal_To("word.annotation"          , FP.HOTLOAD_ANNOTATIONS)
                                  ],
                           handlers=[
                               DSL_Handler("sentence"                 , FP.SENTENCE),
                               DSL_Handler("sentence.ends"            , FP.SEN_MACRO),
                               DSL_Handler("sentence.annotation.head" , FP.op_head_annotation),
                               DSL_Handler("sentence.annotation.head" , FP.flatten_annotation),
                               DSL_Handler("sentence.operator"        , FP.op_sentence),
                               DSL_Handler("sentence.plural"          , FP.SEN_PLURAL),
                               DSL_Handler("word.constrained"         , FP.SEN_NO_MODAL),
                               # Query
                               DSL_Handler("operators.query"          , PU.OPERATOR_SUGAR),
                               DSL_Handler("sentence.annotation.post" , QP.query_sen_post_annotation),
                               DSL_Handler("sentence.ends"            , QP.query_statement),
                               DSL_Handler("word.annotation"          , QP.word_query_constraint),
                               DSL_Handler("word.annotation"          , FP.flatten_annotation),
                               # Transform
                               DSL_Handler("operators.transform"      , PU.OPERATOR_SUGAR),
                               DSL_Handler("sentence.ends"            , TP.transform_statement),
                               # Action
                               DSL_Handler("operators.action"         , PU.OPERATOR_SUGAR),
                               DSL_Handler("sentence.ends"            , AP.action_definition),
                               # Rule
                               DSL_Handler("sentence.ends"            , RP.rule),
                               # Total
                               DSL_Handler(DEFAULT_HANDLER_SIGNAL     , TotalP.parse_point)]
                           )
