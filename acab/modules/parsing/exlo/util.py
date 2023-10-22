"""


"""
# pylint: disable=bad-whitespace,unnecessary-comprehension
##-- imports
from __future__ import annotations
import acab.core.defaults.value_keys as DS
import pyparsing as pp
import acab
from acab.core.defaults import parse_symbols as DSym
from acab.core.parsing.consts import CPAR, NG, OPAR, N
from acab.core.parsing.parsers import MODAL
from acab.core.defaults.value_keys import TYPE_BASE as ATOM_V
from acab.core.value.instruction import ProductionContainer
from acab.core.value.sentence import Sentence
from acab.error.parse import AcabParseException
from acab.interfaces.value import ValueFactory

##-- end imports

config = acab.config

ACTION_S          = config.any_of().parse.structure.ACTION
ANNOTATION_S      = config.any_of().parse.structure.ANNOTATION
BIND_S            = config.any_of().parse.structure.BIND
CONSTRAINT_S      = config.any_of().parse.structure.CONSTRAINT
DEFAULT_ACTION_S  = config.any_of().parse.structure.DEFAULT_ACTION
QUERY_FALLBACK_S  = config.any_of().parse.structure.QUERY_FALLBACK
LEFT_S            = config.any_of().parse.structure.LEFT
NEGATION_S        = config.any_of().parse.structure.NEGATION
OPERATOR_S        = config.any_of().parse.structure.OPERATOR
MODAL_S           = config.any_of().parse.structure.MODAL
QUERY_S           = config.any_of().parse.structure.QUERY
RIGHT_S           = config.any_of().parse.structure.RIGHT
TARGET_S          = config.any_of().parse.structure.TARGET
TRANSFORM_S       = config.any_of().parse.structure.TRANSFORM
VALUE_S           = config.any_of().parse.structure.VALUE
TYPE_INSTANCE_S   = config.any_of().parse.structure.TYPE_INSTANCE

# Core Components
QUERY_COMPONENT     = DS.QUERY_COMPONENT
TRANSFORM_COMPONENT = DS.TRANSFORM_COMPONENT
ACTION_COMPONENT    = DS.ACTION_COMPONENT

STRUCT    : str = config.any_of().types.primitive.STRUCTURE
INSTR     : str = config.any_of().types.primitive.INSTRUCT
CONTAINER : str = config.any_of().types.primitive.CONTAINER
RULE_PRIM : str = config.any_of().types.primitive.RULE

COLLAPSE_CONTEXT = DSym.COLLAPSE_CONTEXT
