"""
Layer Parser

Should define a layer as a combination of rule selectors
and a sequence of agendas

eg:
Γ::a.layer:
	a.rule.selector.$x?
	another.rule.selector.$y?

	primary.agenda.$x
	secondary.agenda.$y

end

"""
import logging as root_logger
import pyparsing as pp
from py_rule.util import QUERY_S, TRANSFORM_S, ACTION_S
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.production_operator import ProductionContainer
from py_rule.abstract.layer import Layer, make_layer

logging = root_logger.getLogger(__name__)


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

def parseString(s):
    return parse_point.parseString(s)
