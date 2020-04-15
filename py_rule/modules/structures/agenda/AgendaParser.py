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

def construct_agenda(toks):
    # construct initializer dictionary
    the_dict = {}

    return ("agenda_dict", the_dict)


HOTLOAD_BASIC_SEN = pp.Forward()

# TODO: an agenda is a special form of rule
conditions  = PU.N(WMU.CONDITION_S, QP.clauses + PU.gap)
transforms  = PU.N(WMU.TRANSFORM_S, TP.transforms + PU.gap)
var_setting = PU.NG(WMU.ACTION_S, AP.actions + PU.component_gap)

agenda_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

agenda_stmt = PU.STATEMENT_CONSTRUCTOR(PU.AGENDA_HEAD,
                                       HOTLOAD_BASIC_SEN,
                                       agenda_body,
                                       parse_fn=make_agenda)

agenda_body.setParseAction(construct_agenda)


parse_point = agenda_stmt

def parseString(s):
    return parse_point.parseString(s)
