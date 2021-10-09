"""


"""
# pylint: disable=bad-whitespace,unnecessary-comprehension
import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.data.default_structure import TYPE_BOTTOM_NAME as ATOM_V
from acab.core.data.production_abstractions import ProductionContainer
from acab.core.data.values import AcabValue, Sentence
from acab.core.parsing.consts import CPAR, NG, OPAR, N
from acab.core.parsing.parsers import MODAL
from acab.error.parse_exception import AcabParseException

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

RULE_HEAD        = config.prepare("Aliases", "RULE")()
QUERY_HEAD       = config.prepare("Aliases", "QUERY")()
TRANSFORM_HEAD   = config.prepare("Aliases", "TRANSFORM")()
ACTION_HEAD      = config.prepare("Aliases", "ACTION")()
FACT_HEAD        = config.prepare("Aliases", "FACT")()
AGENDA_HEAD      = config.prepare("Aliases", "AGENDA")()
LAYER_HEAD       = config.prepare("Aliases", "LAYER")()
PIPE_HEAD        = config.prepare("Aliases", "PIPE")()
COLLAPSE_CONTEXT = config.prepare("Aliases", "CTX_COLLAPSE")()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
