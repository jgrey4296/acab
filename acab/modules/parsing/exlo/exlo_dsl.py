"""
DSL Interface for exclusion logic, to connect it into Acab
"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
import pyparsing as pp

from acab.interfaces.dsl import DSL_Fragment, DSL_Spec, DSL_Handler
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


EXLO_Parser = DSL_Fragment(specs=[DSL_Spec("word.value"          , struct=PU.HOTLOAD_VALUES,            flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("word.annotation"     , struct=FP.HOTLOAD_ANNOTATIONS,       flags=[DSL_Spec.flag_e.COLLECT]),
                                  # DSL_Spec(TODO FP.HOTLOAD_SEN_HEADS
                                  DSL_Spec("sentence.ends"       , struct=FP.HOTLOAD_SEN_ENDS,          flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("operator.query"      , struct=QP.HOTLOAD_QUERY_OP,          flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("query.statement"     , struct=QP.HOTLOAD_QUERY_SEN,         flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("operators.transform" , struct=TP.HOTLOAD_TRANS_OP,          flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("transform.statement" , struct=TP.HOTLOAD_TRANS_STATEMENTS,  flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("operators.action"    , struct=AP.HOTLOAD_OPERATORS,         flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("action.statement"    , struct=AP.HOTLOAD_ACTION_STATEMENTS, flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("statement"           , struct=TotalP.HOTLOAD_STATEMENTS,    flags=[DSL_Spec.flag_e.COLLECT]),
                                  DSL_Spec("disallowed.words"    , struct=FP.HOTLOAD_BAD_HEADS,         flags=[DSL_Spec.flag_e.COLLECT])],
                           handlers=[DSL_Handler("word.valbind"      , PU.VALBIND),
                                     DSL_Handler("word.constrained"  , FP.SEN_NO_MODAL),
                                     DSL_Handler("sentence"          , FP.SENTENCE),
                                     DSL_Handler("sentence.ends"     , FP.SEN_MACRO),
                                     DSL_Handler("sentence.operator" , FP.op_sentence),
                                     DSL_Handler("operator.sugar"    , PU.OPERATOR_SUGAR),
                                     DSL_Handler("sentence.plural"   , FP.SEN_PLURAL),
                                     # Query
                                     DSL_Handler("sentence.ends"     , QP.query_statement),
                                     DSL_Handler("sentence.ends"     , QP.query_sen_end),
                                     DSL_Handler("word.annotation"   , QP.word_query_constraint),
                                     # Transform
                                     DSL_Handler("sentence.ends"     , TP.transform_statement),
                                     # Action
                                     DSL_Handler("sentence.ends"     , AP.action_definition),
                                     # Rule
                                     DSL_Handler("sentence.ends"     , RP.rule),
                                     # Total
                                     DSL_Handler("_:_default"        , TotalP.parse_point)]
                           )
