"""


"""
# pylint: disable=bad-whitespace,unnecessary-comprehension
import acab.core.defaults.value_keys as DS
import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.defaults import parse_symbols as DSym
from acab.core.parsing.consts import CPAR, NG, OPAR, N
from acab.core.parsing.parsers import MODAL
from acab.core.defaults.value_keys import TYPE_BASE as ATOM_V
from acab.core.value.instruction import ProductionContainer
from acab.core.value.sentence import Sentence
from acab.error.parse import AcabParseException
from acab.interfaces.value import ValueFactory

config = AcabConfig()

ACTION_S          = config.attr.Parse.Structure.ACTION
ANNOTATION_S      = config.attr.Parse.Structure.ANNOTATION
BIND_S            = config.attr.Parse.Structure.BIND
CONSTRAINT_S      = config.attr.Parse.Structure.CONSTRAINT
DEFAULT_ACTION_S  = config.attr.Parse.Structure.DEFAULT_ACTION
QUERY_FALLBACK_S  = config.attr.Parse.Structure.QUERY_FALLBACK
LEFT_S            = config.attr.Parse.Structure.LEFT
NEGATION_S        = config.attr.Parse.Structure.NEGATION
OPERATOR_S        = config.attr.Parse.Structure.OPERATOR
MODAL_S           = config.attr.Parse.Structure.MODAL
QUERY_S           = config.attr.Parse.Structure.QUERY
RIGHT_S           = config.attr.Parse.Structure.RIGHT
TARGET_S          = config.attr.Parse.Structure.TARGET
TRANSFORM_S       = config.attr.Parse.Structure.TRANSFORM
VALUE_S           = config.attr.Parse.Structure.VALUE
TYPE_INSTANCE_S   = config.attr.Parse.Structure.TYPE_INSTANCE

# Core Components
QUERY_COMPONENT     = DS.QUERY_COMPONENT
TRANSFORM_COMPONENT = DS.TRANSFORM_COMPONENT
ACTION_COMPONENT    = DS.ACTION_COMPONENT

STRUCT    : str = config.attr.Type.Primitive.STRUCTURE
INSTR     : str = config.attr.Type.Primitive.INSTRUCT
CONTAINER : str = config.attr.Type.Primitive.CONTAINER
RULE_PRIM : str = config.attr.Type.Primitive.RULE

COLLAPSE_CONTEXT = DSym.COLLAPSE_CONTEXT
