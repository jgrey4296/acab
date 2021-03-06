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

from acab.config import AcabConfig

from acab.abstract.parsing import util as PU
from acab.abstract.rule.production_operator import ProductionContainer
from acab.abstract.pipeline.layer import Layer, make_layer

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()
QUERY_S = util("Parsing.Structure", "QUERY_S")
TRANSFORM_S = util("Parsing.Structure", "TRANSFORM_S")
ACTION_S = util("Parsing.Structure", "ACTION_S")

HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION = pp.Forward()

# Layers should be a special case of rule
conditions  = PU.N(QUERY_S , HOTLOAD_QUERY     + PU.gap)
transforms  = PU.N(TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = PU.NG(ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

layer_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

layer_stmt = PU.STATEMENT_CONSTRUCTOR(PU.LAYER_HEAD,
                                      HOTLOAD_BASIC_SEN,
                                      layer_body)

layer_body.setParseAction(make_layer)

parse_point = layer_stmt
# parse_point.setFailAction(lambda s, loc, expr, err: print("{}\n{}".format(str(err), err.markInputline())))

def parseString(s):
    return parse_point.parseString(s)
