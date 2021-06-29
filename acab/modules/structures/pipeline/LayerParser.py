"""
Layer Parser

Should define a layer as a combination of rule selectors
and a sequence of agendas

eg:
Γ::a.layer:
	| $pipeline_var |

	;; *must* only produce a single context?
	an.agenda.set.$agenda(::Σ)?
	a.rule.selector.$x?
	another.rule.selector.$y?

	dfs(@x, (::ρ, #a_tag))      -> $z(::[ρ])
	leaves(@y, (::ρ))           -> $q(::[ρ])
	merge($z, $q)               -> $i
	LayerRunRules($i)           -> $b
	LayerRunAgenda($agenda, $b) -> $c

	LayerPerform($c)
end

"""
import logging as root_logger
import pyparsing as pp

from acab.abstract.config.config import AcabConfig

from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import LAYER_HEAD
from acab.abstract.parsing.funcs import make_layer
from acab.abstract.core.production_abstractions import ProductionContainer

logging = root_logger.getLogger(__name__)

config        = AcabConfig.Get()
QUERY_S     = config.prepare("Parse.Structure", "QUERY")()
TRANSFORM_S = config.prepare("Parse.Structure", "TRANSFORM")()
ACTION_S    = config.prepare("Parse.Structure", "ACTION")()

HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION = pp.Forward()

# Layers should be a special case of rule
conditions  = PU.N(QUERY_S , HOTLOAD_QUERY     + PU.gap)
transforms  = PU.N(TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = PU.NG(ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

layer_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

layer_stmt = PU.STATEMENT_CONSTRUCTOR(LAYER_HEAD,
                                      HOTLOAD_BASIC_SEN,
                                      layer_body)

layer_body.setParseAction(make_layer)

parse_point = layer_stmt
# parse_point.setFailAction(lambda s, loc, expr, err: print("{}\n{}".format(str(err), err.markInputline())))

def parseString(s):
    return parse_point.parseString(s)
