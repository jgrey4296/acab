"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
# pylint: disable=bad-whitespace,unnecessary-comprehension
import pyparsing as pp

from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import N, NG
from acab.abstract.parsing.consts import OPAR, CPAR, ATOM_V
from acab.abstract.parsing.parsers import MODAL
from acab.abstract.parsing import funcs as Pfunc

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence

from acab.abstract.containers.production_abstractions import ProductionContainer

from acab.error.acab_parse_exception import AcabParseException
from acab.working_memory.trie_wm import util as WMU

config = AcabConfig.Get()


AT_BIND_S         = config.value("Value.Structure", "BIND")
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