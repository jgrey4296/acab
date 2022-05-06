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
import logging as logmod

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.value.instruction import ProductionContainer
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import LAYER_HEAD
from acab.core.parsing.funcs import make_layer
from acab.core.parsing.statement_core import StatementCore

logging = logmod.getLogger(__name__)

config        = AcabConfig()
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

layer_stmt = StatementCore(HOTLOAD_BASIC_SEN,
                           layer_body)

layer_body.set_parse_action(make_layer)

parse_point = layer_stmt
# parse_point.set_fail_action(lambda s, loc, expr, err: print("{}\n{}".format(str(err), err.markInputline())))

def parse_string(s):
    return parse_point.parse_string(s)
