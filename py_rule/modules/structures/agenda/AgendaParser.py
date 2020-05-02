"""
Agenda Parser

Should define an Agenda Instance with its parameters,
to then be used in a layer

σ::DefaultAgenda:
	select.$x(::Number)
end

σ::RankingAgenda:
	rank.by.$x
end

eg:
Σ::primary.agenda(::DefaultAgenda): select.20 end
Σ::secondary.agenda(::RankingAgenda): rank.by:$x? end

--------------------
σ::Number
σ::DefaultAgenda:
  selection.amount: $x(::Number)


Σ::primary.agenda(::DefaultAgenda):
  # Query part:
  some.amount!$x(::Number)?

  # Assignment
  rank.by.$x


Σ::secondary.agenda(::RankingAgenda):
  # Use an already bound var from the rules
  rank.by.$x
  # Or:
  curve.sine($y) -> $z

  rank.by.$z(::ordinal)

"""
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.production_operator import ProductionContainer
from py_rule.abstract.agenda import Agenda, make_agenda

logging = root_logger.getLogger(__name__)

HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION = pp.Forward()


# agenda should be a special case of rule
conditions  = PU.N(util.QUERY_S , HOTLOAD_QUERY     + PU.gap)
transforms  = PU.N(util.TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = PU.NG(util.ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

agenda_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

agenda_stmt = PU.STATEMENT_CONSTRUCTOR(PU.AGENDA_HEAD,
                                       HOTLOAD_BASIC_SEN,
                                       agenda_body)

agenda_body.setParseAction(make_agenda)


parse_point = agenda_stmt

def parseString(s):
    return parse_point.parseString(s)
