"""


"""
import acab.core.data.default_structure as DS
# pylint: disable=bad-whitespace,unnecessary-comprehension
import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.data.default_structure import TYPE_BASE as ATOM_V
from acab.core.data.instruction import ProductionContainer
from acab.core.data.value import AcabValue
from acab.core.data.sentence import  Sentence
from acab.core.parsing import default_symbols as DSym
from acab.core.parsing.consts import CPAR, NG, OPAR, N
from acab.core.parsing.parsers import MODAL
from acab.error.parse import AcabParseException
from acab.interfaces.value import ValueFactory_i
config = AcabConfig()

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
QUERY_COMPONENT     = DS.QUERY_COMPONENT
TRANSFORM_COMPONENT = DS.TRANSFORM_COMPONENT
ACTION_COMPONENT    = DS.ACTION_COMPONENT

RULE_PRIM           : str = config.prepare("Type.Primitive", "RULE")()

QUERY_SEM_HINT     = ValueFactory_i.sen([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = ValueFactory_i.sen([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = ValueFactory_i.sen([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = ValueFactory_i.sen([config.prepare("SEMANTICS", "RULE")()])

RULE_HEAD        = DSym.AliasDict["RULE"]
QUERY_HEAD       = DSym.AliasDict["QUERY"]
TRANSFORM_HEAD   = DSym.AliasDict["TRANSFORM"]
ACTION_HEAD      = DSym.AliasDict["ACTION"]
FACT_HEAD        = DSym.AliasDict["FACT"]
AGENDA_HEAD      = DSym.AliasDict["AGENDA"]
LAYER_HEAD       = DSym.AliasDict["LAYER"]
PIPE_HEAD        = DSym.AliasDict["PIPE"]
COLLAPSE_CONTEXT = DSym.AliasDict["CTX_COLLAPSE"]

