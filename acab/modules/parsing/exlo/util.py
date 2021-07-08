"""


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

ACTION_S          = config.prepare("Parse.Structure", "ACTION")()
ANNOTATION_S      = config.prepare("Parse.Structure", "ANNOTATION")()
BIND_S            = config.prepare("Parse.Structure", "BIND")()
CONSTRAINT_S      = config.prepare("Parse.Structure", "CONSTRAINT")()
DEFAULT_ACTION_S  = config.prepare("Parse.Structure", "DEFAULT_ACTION")()
QUERY_FALLBACK_S  = config.prepare("Parse.Structure", "QUERY_FALLBACK")()
LEFT_S            = config.prepare("Parse.Structure", "LEFT")()
NEGATION_S        = config.prepare("Parse.Structure", "NEGATION")()
NODE_S            = config.prepare("Parse.Structure", "NODE")()
OPERATOR_S        = config.prepare("Parse.Structure", "OPERATOR")()
MODAL_S           = config.prepare("Parse.Structure", "MODAL")()
QUERY_S           = config.prepare("Parse.Structure", "QUERY")()
RIGHT_S           = config.prepare("Parse.Structure", "RIGHT")()
TARGET_S          = config.prepare("Parse.Structure", "TARGET")()
TRANSFORM_S       = config.prepare("Parse.Structure", "TRANSFORM")()
VALUE_S           = config.prepare("Parse.Structure", "VALUE")()
TYPE_INSTANCE_S   = config.prepare("Parse.Structure", "TYPE_INSTANCE")()

# Core Components
QUERY_COMPONENT     : str = config.prepare("Structure.Components", "QUERY")()
TRANSFORM_COMPONENT : str = config.prepare("Structure.Components", "TRANSFORM")()
ACTION_COMPONENT    : str = config.prepare("Structure.Components", "ACTION")()

RULE_PRIM           : str = config.prepare("Type.Primitive", "RULE")()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
