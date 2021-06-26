"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
# pylint: disable=bad-whitespace,unnecessary-comprehension
import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.default_structure import TYPE_BOTTOM_NAME as ATOM_V
from acab.abstract.core.production_abstractions import ProductionContainer
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.parsing.consts import CPAR, NG, OPAR, N
from acab.abstract.parsing.parsers import MODAL
from acab.error.acab_parse_exception import AcabParseException

config = AcabConfig.Get()

ACTION_S          = config.value("Parse.Structure", "ACTION")
ANNOTATION_S      = config.value("Parse.Structure", "ANNOTATION")
BIND_S            = config.value("Parse.Structure", "BIND")
CONSTRAINT_S      = config.value("Parse.Structure", "CONSTRAINT")
DEFAULT_ACTION_S  = config.value("Parse.Structure", "DEFAULT_ACTION")
QUERY_FALLBACK_S  = config.value("Parse.Structure", "QUERY_FALLBACK")
LEFT_S            = config.value("Parse.Structure", "LEFT")
NEGATION_S        = config.value("Parse.Structure", "NEGATION")
NODE_S            = config.value("Parse.Structure", "NODE")
OPERATOR_S        = config.value("Parse.Structure", "OPERATOR")
MODAL_S           = config.value("Parse.Structure", "MODAL")
QUERY_S           = config.value("Parse.Structure", "QUERY")
RIGHT_S           = config.value("Parse.Structure", "RIGHT")
TARGET_S          = config.value("Parse.Structure", "TARGET")
TRANSFORM_S       = config.value("Parse.Structure", "TRANSFORM")
VALUE_S           = config.value("Parse.Structure", "VALUE")
TYPE_INSTANCE_S   = config.value("Parse.Structure", "TYPE_INSTANCE")

QUERY_SEM_HINT     = Sentence.build([config.value("SEMANTICS", "QUERY")])
ACTION_SEM_HINT    = Sentence.build([config.value("SEMANTICS", "ACTION")])
TRANSFORM_SEM_HINT = Sentence.build([config.value("SEMANTICS", "TRANSFORM")])
RULE_SEM_HINT      = Sentence.build([config.value("SEMANTICS", "RULE")])
