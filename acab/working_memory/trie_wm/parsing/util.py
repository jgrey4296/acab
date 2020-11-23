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

from acab.abstract.core.core_abstractions import AcabValue
from acab.abstract.core.core_abstractions import Sentence

from acab.abstract.rule.production_abstractions import ProductionContainer

from acab.error.acab_parse_exception import AcabParseException
from acab.working_memory.trie_wm import util as WMU

util = AcabConfig.Get()


AT_BIND_S         = util.value("Value.Structure", "BIND")
ACTION_S          = util.value("Parse.Structure", "ACTION")
ANNOTATION_S      = util.value("Parse.Structure", "ANNOTATION")
BIND_S            = util.value("Parse.Structure", "BIND")
CONSTRAINT_S      = util.value("Parse.Structure", "CONSTRAINT")
DEFAULT_ACTION_S  = util.value("Parse.Structure", "DEFAULT_ACTION")
QUERY_FALLBACK_S  = util.value("Parse.Structure", "QUERY_FALLBACK")
LEFT_S            = util.value("Parse.Structure", "LEFT")
NEGATION_S        = util.value("Parse.Structure", "NEGATION")
NODE_S            = util.value("Parse.Structure", "NODE")
OPERATOR_S        = util.value("Parse.Structure", "OPERATOR")
MODAL_S           = util.value("Parse.Structure", "MODAL")
QUERY_S           = util.value("Parse.Structure", "QUERY")
RIGHT_S           = util.value("Parse.Structure", "RIGHT")
TARGET_S          = util.value("Parse.Structure", "TARGET")
TRANSFORM_S       = util.value("Parse.Structure", "TRANSFORM")
VALUE_S           = util.value("Parse.Structure", "VALUE")
TYPE_INSTANCE_S   = util.value("Parse.Structure", "TYPE_INSTANCE")
